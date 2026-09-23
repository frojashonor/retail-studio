#!/usr/bin/env python3
"""
Comprueba que las salidas impresas en el libro coinciden con lo que R produce.

Para cada bloque rcode que va seguido inmediatamente de un bloque rsalida,
ejecuta el código (todos los bloques en orden, en una sola sesión de R, con
el paquete evaluate) y verifica que cada línea de la salida del libro aparece,
en el mismo orden, en la salida real. Una línea que contiene solo "..." en el
libro marca un recorte y se ignora.

Uso:
    python3 herramientas/verificar_salidas.py DIR_TRABAJO capitulos/04-analisis-exploratorio.tex

DIR_TRABAJO debe contener la carpeta datasets/ con los CSV generados.
"""
import json
import os
import re
import subprocess
import sys

BLOQUE = re.compile(
    r"\\begin\{(rcode|rsalida)\}(\[[^\n]*\])?[ \t]*\n(.*?)\\end\{\1\}", re.S
)


def bloques(ruta):
    texto = open(ruta, encoding="utf-8").read()
    res = []
    for m in BLOQUE.finditer(texto):
        res.append({
            "tipo": m.group(1),
            "linea": texto.count("\n", 0, m.start()) + 1,
            "inicio": m.start(),
            "fin": m.end(),
            "contenido": m.group(3),
        })
    # Asociar cada rsalida al rcode inmediatamente anterior (solo espacios entre ambos)
    codigo = []
    for i, b in enumerate(res):
        if b["tipo"] != "rcode":
            continue
        esperado = None
        if i + 1 < len(res) and res[i + 1]["tipo"] == "rsalida":
            entre = texto[b["fin"]:res[i + 1]["inicio"]]
            if entre.strip() == "":
                esperado = res[i + 1]["contenido"]
        codigo.append({"linea": b["linea"], "codigo": b["contenido"],
                       "esperado": esperado})
    return codigo


SCRIPT_R = r'''
options(width = 70, cli.unicode = FALSE, crayon.enabled = FALSE)
# Todo el estado del verificador vive en .verif para no chocar con las
# variables del capítulo (i, r, res, salida...), que se evalúan en globalenv().
.verif <- new.env()
local(envir = .verif, {
  args <- commandArgs(TRUE)
  bloques <- jsonlite::fromJSON(args[1], simplifyVector = FALSE)
  salida <- list()
  for (i in seq_along(bloques)) {
    res <- evaluate::evaluate(bloques[[i]]$codigo, envir = globalenv(),
                              stop_on_error = 1L, new_device = FALSE)
    txt <- character()
    error <- NULL
    asegurar_salto <- function(x) ifelse(grepl("\n$", x), x, paste0(x, "\n"))
    for (r in res) {
      if (evaluate::is.source(r)) next
      if (is.character(r)) txt <- c(txt, r)
      else if (evaluate::is.message(r)) txt <- c(txt, asegurar_salto(conditionMessage(r)))
      else if (evaluate::is.warning(r)) txt <- c(txt, paste0("Warning message:\n", conditionMessage(r), "\n"))
      else if (evaluate::is.error(r)) { error <- conditionMessage(r); txt <- c(txt, paste0("Error: ", error, "\n")) }
    }
    salida[[i]] <- list(texto = paste(txt, collapse = ""), error = error)
    if (!is.null(error)) break
  }
  writeLines(jsonlite::toJSON(salida, auto_unbox = TRUE, null = "null"), args[2])
})
'''


def normalizar(linea):
    n = re.sub(r"\s+", " ", linea).strip()
    # Acepta advertencias en formato corto ("Warning: msg") o largo
    # ("Warning message:" y el mensaje en la línea siguiente).
    return n[len("Warning: "):] if n.startswith("Warning: ") else n


def comparar(esperado, real):
    """Cada línea esperada (no vacía, no '...') debe aparecer en orden en la real."""
    reales = [normalizar(l) for l in real.splitlines() if normalizar(l)]
    j = 0
    faltan = []
    for l in esperado.splitlines():
        n = normalizar(l)
        if not n or n in ("...", "…"):
            continue
        k = j
        while k < len(reales) and reales[k] != n:
            k += 1
        if k == len(reales):
            faltan.append(l.rstrip())
        else:
            j = k + 1
    return faltan


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(2)
    dir_trabajo = sys.argv[1]
    total_fallos = 0
    for ruta in sys.argv[2:]:
        cod = bloques(ruta)
        base = os.path.splitext(os.path.basename(ruta))[0]
        entrada = os.path.join(dir_trabajo, f"_bloques_{base}.json")
        salida = os.path.join(dir_trabajo, f"_salidas_{base}.json")
        script = os.path.join(dir_trabajo, "_evaluar.R")
        json.dump([{"codigo": c["codigo"]} for c in cod],
                  open(entrada, "w", encoding="utf-8"))
        open(script, "w", encoding="utf-8").write(SCRIPT_R)
        entorno = dict(os.environ, LANG="es_MX.UTF-8", LC_ALL="es_MX.UTF-8")
        r = subprocess.run(["Rscript", "--vanilla", "_evaluar.R",
                            os.path.basename(entrada), os.path.basename(salida)],
                           cwd=dir_trabajo, capture_output=True, text=True,
                           env=entorno)
        if not os.path.exists(salida):
            print(f"{base}: no se pudo evaluar\n{r.stderr[-3000:]}")
            total_fallos += 1
            continue
        reales = json.load(open(salida, encoding="utf-8"))
        comparadas = fallos = 0
        for i, c in enumerate(cod):
            if i >= len(reales):
                print(f"  [línea {c['linea']}] no se ejecutó (error previo)")
                fallos += 1
                break
            real = reales[i]
            if real.get("error"):
                print(f"  [línea {c['linea']}] ERROR al ejecutar: {real['error']}")
                fallos += 1
                break
            if c["esperado"] is None:
                continue
            comparadas += 1
            faltan = comparar(c["esperado"], real["texto"])
            if faltan:
                fallos += 1
                print(f"  [línea {c['linea']}] la salida del libro no coincide.")
                print("    Líneas del libro que no aparecen en la salida real:")
                for l in faltan[:8]:
                    print("      | " + l)
                print("    Salida real:")
                for l in real["texto"].splitlines()[:25]:
                    print("      > " + l)
        estado = "OK" if fallos == 0 else f"{fallos} PROBLEMA(S)"
        print(f"{base}: {len(cod)} bloques rcode, {comparadas} salidas comparadas -> {estado}")
        total_fallos += fallos
    sys.exit(1 if total_fallos else 0)


if __name__ == "__main__":
    main()

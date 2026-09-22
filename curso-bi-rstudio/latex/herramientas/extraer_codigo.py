#!/usr/bin/env python3
"""
Extrae el código R de los capítulos del libro.

Cada bloque \\begin{rcode} ... \\end{rcode} de un capítulo se copia, en orden,
a un script .R. Así puedes ejecutar todo el código de un módulo de corrido
en RStudio sin copiar y pegar desde el PDF.

Uso:
    # Generar los scripts de todos los módulos en la carpeta codigo/
    python3 herramientas/extraer_codigo.py

    # Generar y además ejecutar un capítulo para comprobar que no falla
    # (DIR_TRABAJO debe contener la carpeta datasets/ con los CSV generados)
    python3 herramientas/extraer_codigo.py --ejecutar DIR_TRABAJO capitulos/02-manipulacion-datos.tex

Los bloques \\begin{rnoejecutar} (instalaciones, apps Shiny, conexiones a
servidores externos) se incluyen comentados, solo como referencia.
"""
import argparse
import os
import re
import subprocess
import sys

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PATRON = re.compile(
    r"\\begin\{(rcode|rnoejecutar)\}(\[[^\n]*\])?[ \t]*\n(.*?)\\end\{\1\}",
    re.S,
)
TITULO = re.compile(r"\\chapter\*?\{([^}]*)\}")


def extraer(ruta_tex):
    texto = open(ruta_tex, encoding="utf-8").read()
    m = TITULO.search(texto)
    titulo = m.group(1) if m else os.path.basename(ruta_tex)
    bloques = []
    for m in PATRON.finditer(texto):
        linea = texto.count("\n", 0, m.start()) + 1
        bloques.append((m.group(1), linea, m.group(3).rstrip() + "\n"))
    return titulo, bloques


def script_lectura(ruta_tex, titulo, bloques):
    """Script limpio para el lector."""
    nombre = os.path.basename(ruta_tex)
    partes = [
        "# " + "=" * 74,
        f"# {titulo}",
        f"# Código del libro 'Business Intelligence con R y RStudio'",
        f"# Generado automáticamente a partir de capitulos/{nombre}",
        "# Ejecuta este script con el directorio de trabajo en la carpeta",
        "# del curso (la que contiene datasets/).",
        "# " + "=" * 74,
        "",
    ]
    n = 0
    for tipo, _linea, codigo in bloques:
        if tipo == "rcode":
            n += 1
            partes.append(f"# ---- Bloque {n} " + "-" * 56)
            partes.append(codigo)
        else:
            partes.append("# ---- Solo referencia (no se ejecuta automáticamente) ----")
            partes.extend("# " + l if l.strip() else "#" for l in codigo.splitlines())
            partes.append("")
    return "\n".join(partes)


def script_verificacion(bloques):
    """Script con marcas para saber qué bloque falla."""
    partes = [
        "options(width = 70, cli.unicode = FALSE, crayon.enabled = FALSE)",
        ".bloque <- function(n, l) cat(sprintf('\\n#### BLOQUE %d (linea %d)\\n', n, l))",
    ]
    n = 0
    for tipo, linea, codigo in bloques:
        if tipo != "rcode":
            continue
        n += 1
        partes.append(f".bloque({n}, {linea})")
        partes.append(codigo)
    partes.append("cat('\\n#### FIN SIN ERRORES\\n')")
    return "\n".join(partes)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("capitulos", nargs="*")
    ap.add_argument("--ejecutar", metavar="DIR_TRABAJO")
    args = ap.parse_args()

    caps = args.capitulos or sorted(
        os.path.join(RAIZ, "capitulos", f)
        for f in os.listdir(os.path.join(RAIZ, "capitulos"))
        if f.endswith(".tex")
    )
    os.makedirs(os.path.join(RAIZ, "codigo"), exist_ok=True)
    fallos = 0
    for cap in caps:
        titulo, bloques = extraer(cap)
        n_run = sum(1 for b in bloques if b[0] == "rcode")
        if not bloques:
            continue
        base = os.path.splitext(os.path.basename(cap))[0]
        destino = os.path.join(RAIZ, "codigo", base + ".R")
        with open(destino, "w", encoding="utf-8") as f:
            f.write(script_lectura(cap, titulo, bloques))
        print(f"{base}: {n_run} bloques ejecutables -> codigo/{base}.R")

        if args.ejecutar:
            ver = os.path.join(args.ejecutar, f"_verificar_{base}.R")
            with open(ver, "w", encoding="utf-8") as f:
                f.write(script_verificacion(bloques))
            entorno = dict(os.environ, LANG="es_MX.UTF-8", LC_ALL="es_MX.UTF-8")
            r = subprocess.run(
                ["Rscript", "--vanilla", os.path.basename(ver)],
                cwd=args.ejecutar, capture_output=True, text=True, env=entorno,
            )
            salida = r.stdout + r.stderr
            if r.returncode != 0 or "FIN SIN ERRORES" not in salida:
                fallos += 1
                marcas = re.findall(r"#### BLOQUE (\d+) \(linea (\d+)\)", salida)
                ult = marcas[-1] if marcas else ("?", "?")
                print(f"  ERROR en bloque {ult[0]} (línea {ult[1]} de {base}.tex)")
                print("  " + "\n  ".join(salida.strip().splitlines()[-25:]))
            else:
                print("  OK: todo el código se ejecutó sin errores")
    sys.exit(1 if fallos else 0)


if __name__ == "__main__":
    main()

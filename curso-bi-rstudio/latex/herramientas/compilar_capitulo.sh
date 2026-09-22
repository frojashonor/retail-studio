#!/usr/bin/env bash
# ============================================================================
# Compila UN solo módulo del libro (más sus soluciones) para revisarlo rápido.
#
# Uso:
#   herramientas/compilar_capitulo.sh capitulos/03-visualizacion.tex \
#                                     [apendices/soluciones/sol-03.tex]
#
# Deja el PDF en $DIR_COMPILACION/compilar-<capitulo>/prueba.pdf
# (por defecto DIR_COMPILACION=/tmp) y muestra errores, referencias
# indefinidas y cajas desbordadas (overfull) mayores a 5pt.
# ============================================================================
set -u
RAIZ=$(cd "$(dirname "$0")/.." && pwd)
CAP=$1
SOL=${2:-}
BASE=$(basename "$CAP" .tex)
NUM=$((10#${BASE:0:2}))
TMP=${DIR_COMPILACION:-/tmp}/compilar-$BASE
rm -rf "$TMP"; mkdir -p "$TMP"
cp "$RAIZ/preambulo.tex" "$TMP/"
ln -s "$RAIZ/figuras" "$TMP/figuras"

CAP_ABS=$(cd "$(dirname "$CAP")" && pwd)/$(basename "$CAP" .tex)
{
  echo '\documentclass[11pt,oneside,openany]{book}'
  echo '\input{preambulo}'
  echo '\begin{document}'
  if [ "$NUM" -eq 0 ]; then
    echo '\frontmatter'
  else
    echo '\mainmatter'
    echo "\\setcounter{chapter}{$((NUM - 1))}"
  fi
  echo "\\input{$CAP_ABS}"
  if [ -n "$SOL" ]; then
    SOL_ABS=$(cd "$(dirname "$SOL")" && pwd)/$(basename "$SOL" .tex)
    echo '\appendix'
    echo '\chapter{Soluciones (vista previa)}'
    echo "\\input{$SOL_ABS}"
  fi
  echo '\end{document}'
} > "$TMP/prueba.tex"

cd "$TMP"
latexmk -pdf -interaction=nonstopmode -halt-on-error prueba.tex > build.log 2>&1
ESTADO=$?

echo "== Compilación de $BASE: $( [ $ESTADO -eq 0 ] && echo OK || echo FALLÓ )"
if [ $ESTADO -ne 0 ]; then
  grep -n -A6 '^!' prueba.log | head -40
fi
echo "== Referencias indefinidas (se ignoran las de otros módulos: cap:, ap:, sec: de otros archivos)"
grep -o "Reference \`[^']*' on page [0-9]* undefined" prueba.log | sort -u | head -30
echo "== Cajas desbordadas > 5pt (línea del archivo en el log)"
grep -A1 -E 'Overfull \\hbox \(([5-9]|[1-9][0-9]+)\.[0-9]+pt too wide\)' prueba.log | head -40
echo "== Advertencias de fuentes o caracteres"
grep -E 'Missing character|Unicode character|Font shape .* undefined' prueba.log | sort -u | head -20
if [ -f prueba.pdf ]; then
  echo "== Páginas: $(pdfinfo prueba.pdf 2>/dev/null | awk '/Pages/{print $2}')  ->  $TMP/prueba.pdf"
fi
exit $ESTADO

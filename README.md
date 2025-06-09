# Proyecto: Evidencia 2 - Recetas Funcionales

## Cómo ejecutar
1. Abrir un REPL en el proyecto.
2. Ejecutar:
   (require '[evidencia2receta.core :refer [procesar-multiples-recetas]])
   (procesar-multiples-recetas)

## Qué hace
- Lee recetas desde resources.
- Aplica filtros, conversiones, escalado y genera .html.
- Usa configuraciones desde opciones1.txt.

Sin uso de librerias
# Leer el archivo original
with open('2022_consultas.txt', 'r') as file:
    lineas = file.readlines()


print(lineas)


# # Crear un nuevo archivo con los valores incrementales
# with open('datos_modificados.txt', 'w') as file:
#     incremento = 0  # Valor inicial
#     for linea in lineas:
#         linea = linea.strip()  # Eliminar espacios en blanco
#         if linea:  # Verificar que la línea no esté vacía
#             file.write(f'"{incremento}",{linea}\n')  # Escribir en el archivo
#             incremento += 1  # Incrementar el valor

# print("Archivo modificado guardado como 'datos_modificados.txt'")

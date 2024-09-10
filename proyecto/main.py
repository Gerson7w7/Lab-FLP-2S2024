import tkinter as tk
import subprocess

def enviar_datos():
    # Obtener el dato ingresado en la entrada
    dato = entry.get()
    
    # Ejecutar el programa Fortran y enviar el dato
    resultado = subprocess.run(
        ["./analizador.exe"],  # Ejecutable compilado
        input=dato,  # Enviar el dato como cadena de texto
        stdout=subprocess.PIPE,  # Capturar la salida del programa
        text=True  # Asegurarse de que la salida se maneje como texto
    )

    # Mostrar la salida en el área de texto
    output_area.insert(tk.END, resultado.stdout)

# Crear la ventana principal
ventana = tk.Tk()
ventana.title("Interfaz Tkinter con Fortran")

# Crear campo de entrada
tk.Label(ventana, text="Ingrese un valor:").pack()
entry = tk.Entry(ventana)
entry.pack()

# Crear botón para enviar el dato
tk.Button(ventana, text="Enviar a Fortran", command=enviar_datos).pack()

# Crear un área de texto para mostrar la salida
output_area = tk.Text(ventana, height=10, width=40)
output_area.pack()

# Ejecutar la ventana
ventana.mainloop()

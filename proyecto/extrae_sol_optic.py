import os

def busca(l, s):
    for i in range(len(l)):
        if l[i].find(s)!=-1:
            return i
    return -1 # Or -1


def lee():

	nombre1 = '/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/U/metricas.txt'
	nombre2 = '/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/U/estados.txt'
	nombre3 = '/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/U/tiempos.txt'

	metricas = []
	estados = []
	tiempos = []

	path = '/home/juanpablo/Documentos/simulacion/instancias/optic/p/problemas/U/planes/'
	for filename in sorted(os.listdir(path)):

		#EXTRAE LINEAS DE PROBLEMA-PDDL
		with open(path + filename) as file:
			lineas = file.readlines()

			#VALOR DE LAS ACTIVIDADES
			indice_metrica = busca(lineas,'; Plan found with metric')
			indice_estados = busca(lineas,'; States evaluated so far:')
			indice_tiempo = busca(lineas,'; Time')

			if(indice_metrica != -1 & indice_estados != -1 & indice_tiempo != -1):
				linea_metrica = lineas[indice_metrica]
				linea_estados = lineas[indice_estados]
				linea_tiempo = lineas[indice_tiempo]
				print(filename)
				print(linea_metrica)
				print(linea_estados)
				print(linea_tiempo)
			
				sp1 = linea_metrica.rfind(' ')
				sp2 = linea_estados.rfind(' ')
				sp3 = linea_tiempo.rfind(' ')

				linea_metrica = linea_metrica[sp1+1:]
				linea_estados = linea_estados[sp2+1:]
				linea_tiempo = linea_tiempo[sp3+1:]

				metricas.append(linea_metrica)
				estados.append(linea_estados)
				tiempos.append(linea_tiempo)
			else:
				linea_metrica = '0.00\n'
				linea_estados = '0.00\n'
				linea_tiempo = '0.00\n'
				
				print(filename)
				print(linea_metrica)
				print(linea_estados)
				print(linea_tiempo)

				metricas.append(linea_metrica)
				estados.append(linea_estados)
				tiempos.append(linea_tiempo)				

	with open(nombre1, "w") as output:
		for line in metricas:
			output.write(line)

	with open(nombre2, "w") as output:
		for line in estados:
			output.write(line)

	with open(nombre3, "w") as output:
		for line in tiempos:
			output.write(line)


lee()

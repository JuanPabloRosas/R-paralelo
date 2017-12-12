
def busca(l, s):
    for i in range(len(l)):
        if l[i].find(s)!=-1:
            return i
    return -1 # Or -1


def lee(path):
	nombre = '/home/juanpablo/Documentos/simulacion/instancias/simulacion/g/U/datos_pddl/datos/' + path[path.rfind('/')+1:].replace('domain','problem') + '.txt'
	datos = []

	#EXTRAE LINEAS DE PROBLEMA-PDDL
	with open(path) as file:
		lineas = file.readlines()

	#VALOR DE LAS ACTIVIDADES
	indice = busca(lineas,'                (at start (done LA')
	l = ''
	if(indice > 0):
		while(indice != -1):
			linea = lineas[indice]
			sp = linea.rfind(' ')
			linea = linea[sp:]
			linea = linea.replace(' LA','')
			linea = linea.replace('))','')
			lineas.pop(indice)
			indice = busca(lineas,'                (at start (done LA')
			l = l + linea
		datos.append(l)
	
		if os.path.isfile(nombre):
			with open(nombre, 'a+') as output:
				print(nombre)
				for line in datos:
					output.write(line)
	else:
		if os.path.isfile(nombre):
			with open(nombre, 'a+') as output:
				output.write('0')


import glob
import os
path = '/home/juanpablo/Documentos/simulacion/instancias/optic/g/problemas/U/domains/domain*.pddl'
for filename in glob.iglob(path):
	lee(filename)

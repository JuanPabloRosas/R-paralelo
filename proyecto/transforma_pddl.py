
def busca(l, s):
    for i in range(len(l)):
        if l[i].find(s)!=-1:
            return i
    return -1 # Or -1


def lee(path):
	nombre = '/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/' + path[path.rfind('/')+1:] + '.txt'
	datos = []
	materias = 0
	temas = 0
	subtemas = 0
	actividades = 0
	tiene_requisito = 0

	#EXTRAE LINEAS DE PROBLEMA-PDDL
	with open(path) as file:
		lineas = file.readlines()

	#VALOR DE LAS ACTIVIDADES
	indice = busca(lineas,'(= (valueLA LA')
	l = ''
	while(indice != -1):
		actividades = actividades + 1
		linea = lineas[indice]
		sp = linea.rfind(' ')
		linea = linea[sp:]
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		l = l + linea
		lineas.pop(indice)
		indice = busca(lineas,'(= (valueLA LA')
	l = l[1:]
	datos.append(l)

	#DURACION DE LAS ACTIVIDADES
	indice = busca(lineas,'(= (DurationLA LA')
	l = ''
	while(indice != -1):
		linea = lineas[indice].replace('	(= (DurationLA LA','')
		sp = linea.rfind(' ')
		linea = linea[sp:]
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		l = l + linea
		lineas.pop(indice)
		indice = busca(lineas,'(= (DurationLA LA')
	l = l[1:]
	datos.append(l)

	#RECURSOS DE LAS ACTIVIDADES
	indice = busca(lineas,'(KindResourceLO LA')
	l = ''
	while(indice != -1):
		linea = lineas[indice].replace('	(KindResourceLO LA','')
		sp = linea.rfind('rec')
		linea = linea[sp:]
		linea = linea.replace(')','')
		linea = linea.replace('rec','')
		linea = linea.replace('\n','')
		l = l + linea + ' '
		lineas.pop(indice)
		indice = busca(lineas,'(KindResourceLO LA')
	#l = l[0:]
	datos.append(l)

	#ACTIVIDAD ES PARTE DEL SUBTEMA
	indice = busca(lineas,'isPartOfSubtheme LA')
	l = ''
	linea = lineas[indice].replace('	(isPartOfSubtheme LA','')
	actual = linea[linea.rfind('Subtema'):]
	actual = actual.replace('Subtema','')
	actual = actual.replace(')','')	
	while(indice != -1):
		linea = lineas[indice].replace('	(isPartOfSubtheme LA','')
		sp = linea.rfind(' ')
		sp1 = linea.rfind('a')
		pedazo = linea[sp:sp1+1]
		linea = linea.replace(pedazo,' ')
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		sub = linea[(linea.rfind(' ')+1):]
		if(actual == sub):
			linea = linea[:linea.rfind(' ')]
			l = l + linea + ' '
		else:
			subtemas = subtemas + 1
			l = l + '\n'
			linea = linea[:linea.rfind(' ')]
			l = l + linea + ' '
			actual = sub
		lineas.pop(indice)
		indice = busca(lineas,'isPartOfSubtheme LA')
	datos.append(l)

	#SUBTEMA ES PARTE DEL TEMA
	indice = busca(lineas,'	(isPartOfTheme Subtema')
	l = ''
	linea = lineas[indice].replace('	(isPartOfTheme Subtema','')
	actual = linea[linea.rfind('Tema'):]
	actual = actual.replace('Tema','')
	actual = actual.replace(')','')	
	while(indice != -1):
		linea = lineas[indice].replace('	(isPartOfTheme Subtema','')
		sp = linea.rfind(' ')
		sp1 = linea.rfind('a')
		pedazo = linea[sp:sp1+1]
		linea = linea.replace(pedazo,' ')
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		sub = linea[(linea.rfind(' ')+1):]
		if(actual == sub):
			linea = linea[:linea.rfind(' ')]
			l = l + linea + ' '
		else:
			temas = temas + 1
			l = l + '\n'
			linea = linea[:linea.rfind(' ')]
			l = l + linea + ' '
			actual = sub
		lineas.pop(indice)
		indice = busca(lineas,'	(isPartOfTheme Subtema')
	datos.append(l)

	#TEMA ES PARTE DE LA MATERIA
	indice = busca(lineas,'	(isPartOfSubject Tema')
	l = ''
	linea = lineas[indice].replace('	(isPartOfSubject Tema','')
	actual = linea[linea.rfind('Materia'):]
	actual = actual.replace('Materia','')
	actual = actual.replace(')','')	
	while(indice != -1):
		linea = lineas[indice].replace('	(isPartOfSubject Tema','')
		sp = linea.rfind(' ')
		sp1 = linea.rfind('a')
		pedazo = linea[sp:sp1+1]
		linea = linea.replace(pedazo,' ')
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		sub = linea[(linea.rfind(' ')+1):]
		if(actual == sub):
			linea = linea[:linea.rfind(' ')]
			l = l + linea + ' '
		else:
			materias = materias + 1
			l = l + '\n'
			linea = linea[:linea.rfind(' ')]
			l = l + linea + ' '
			actual = sub
		lineas.pop(indice)
		indice = busca(lineas,'	(isPartOfSubject Tema')
	l = l + '\n'
	datos.append(l)

	#TIENE UN REQUISITO
	indice = busca(lineas,'	(has-reqs LA')
	while(indice != -1):
		tiene_requisito = tiene_requisito + 1
		linea = lineas[indice].replace('	(has-reqs LA','')
		linea = lineas[indice].replace('	(has-reqs LA','')
		linea = linea.replace('LA','')
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		lineas.pop(indice)
		indice = busca(lineas,'	(has-reqs LA')
		datos.append(linea)


	#TIENE MAS DE UN REQUISITO
	indice = busca(lineas,'	(has-multiple-reqs LA')
	while(indice != -1):
		tiene_requisito = tiene_requisito + 1
		linea = lineas[indice].replace('	(has-multiple-reqs LA','')
		linea = lineas[indice].replace('	(has-multiple-reqs LA','')
		linea = linea.replace('LA','')
		linea = linea.replace(')','')
		linea = linea.replace('\n','')
		lineas.pop(indice)
		indice = busca(lineas,'	(has-multiple-reqs LA')
		datos.append(linea)


	with open(nombre, "w") as output:
		output.write(str(materias) +'\n')
		output.write(str(temas) +'\n')
		output.write(str(subtemas) +'\n')
		output.write(str(actividades) +'\n')
		output.write(str(tiene_requisito) +'\n')
		for line in datos:
			output.write(line +"\n")


import glob

path = '/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/datos_exp/1parteexp/1ModelosPlanning/problem*.pddl'
for filename in glob.iglob(path):
	#nombre = filename[filename.rfind('/')+1:]
	lee(filename)

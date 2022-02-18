/*****************************************************************************

		Copyright (c) My Company

 Project:  PAPAYAS1
 FileName: PAPAYAS1.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "papayas1.inc"

domains 

  pesototal=real.
  peso=real.
  traza=string.
  dias=real.
  precio=real.
  
  papaya=p(peso,traza,dias)
  listPap=papaya*
  
  bandeja=b(listPap,pesototal,precio)
  listaBandejas=bandeja*
  
predicates

  cobot(listPap,listaBandejas). 
  creaNuevaBandeja(listPap,listaBandejas,bandeja).
  addpapaya(listPap,listaBandejas,bandeja).
  calculaPapaya(papaya,listPap,listaBandejas,bandeja).
  informacionBandejaCompleta(bandeja).
  almacenaPapayasSobrantesAlmacen(listPap).
  
clauses
  	
  	%Primera cláusula que será llamada con la información inicial
  	cobot(ListaDePapayas,ListaDeBandejas):-
  		NewBandeja=b([],0.0,0.0),
  		addpapaya(ListaDePapayas,ListaDeBandejas,NewBandeja).
  		
  	creaNuevaBandeja(ListaPapayas,ListaBandejas,BandejaCompleta):-
  		informacionBandejaCompleta(BandejaCompleta),
  		ListaDeBandejasNueva=[BandejaCompleta|ListaBandejas],
  		NewBandeja=b([],0.0,0.0),
  		addpapaya(ListaPapayas,ListaDeBandejasNueva,NewBandeja).
  		
  	%Añadimos una papaya y la la enviamos a pesar y registrar
  	addpapaya(ListaPapayas,ListaBandejas,b(PapayasDentroAnt,PESOact,PRECIOact)):-
  		PESOact<2.0,
  		ListaPapayas=[H|T],
  		PapayasDentroPost=[H|PapayasDentroAnt],
  		calculaPapaya(H,T,ListaBandejas,b(PapayasDentroPost,PESOact,PRECIOact)).
  	
  	%Si nos quedamos sin papayas y el peso no es suficiente entonces devolvemos las que habíamos cogido al almacen
  	addpapaya(ListaPapayas,_,b(PapayasDentroAnt,PESOact,_)):-
  		PESOact<2.0,
  		ListaPapayas=[H|_],
  		PapayasDentroDes=[H|PapayasDentroAnt],
  		almacenaPapayasSobrantesAlmacen(PapayasDentroDes).
  	
  	%Si el peso total de la bandeja después de meter la papaya es menor que 2kg entonces volvemos a añadir otra papaya
  	calculaPapaya(p(PesoPap,_,DiasAlm),ListaPapayas,ListaBandejas,b(PapayasDentro,PESOact,PRECIOact)):-
  		PESOpost=PESOact+PesoPap,
  		PRECIOpos=PRECIOact+0.10+2*PesoPap+0.05*DiasAlm,
  		PESOpost<2.0, %El control para saber si llenamos la bandeja o no
  		addpapaya(ListaPapayas,ListaBandejas,b(PapayasDentro,PESOpost,PRECIOpos)).
  		
  	%Si el peso total de la bandeja después de meter la papaya es mayor o igual que 2kg entonces creamos una nueva bandeja
  	calculaPapaya(p(PesoPap,_,DiasAlm),ListaPapayas,ListaBandejas,b(PapayasDentro,PESOact,PRECIOact)):-
  		PESOpost=PESOact+PesoPap,
  		PRECIOpos=PRECIOact+0.10+0.3+2*PesoPap+0.05*DiasAlm,
  		PESOpost>=2.0, %El control para saber si llenamos la bandeja o no
  		creaNuevaBandeja(ListaPapayas,ListaBandejas,b(PapayasDentro,PESOpost,PRECIOpos)).
  		
  	informacionBandejaCompleta(b(ListaPapayas,Peso,Precio)):-
  		write("\n\nBANDEJA OBTENIDA: \n PRECIO: ",Precio,"\n PESO: ",Peso,"\n LISTA DE PAPAYAS: ",ListaPapayas,"\n").
  		
  	almacenaPapayasSobrantesAlmacen(PapayasDentroAnt):-
  		write("\n\nPAPAYAS SOBRANTES DEVUELTAS AL ALMACEN: ",PapayasDentroAnt,"\n\n").
  
goal

cobot(	[p(0.273,  "1400001",  1.1), 
	p(0.405,  "1400002",  1.0), 
	p(0.517,  "1400003",  1.1), 
	p(0.533,  "1400004",  1.7), 
	p(0.358,  "1400005",  1.5), 
	p(0.562,  "1400006",  1.9), 
	p(0.322,  "1400007",  2.4), 
	p(0.494,  "1400008",  1.8), 
	p(0.39,   "1400009",  1.6), 
	p(0.281,  "1400010",  2.2), 
	p(0.395,  "1400011",  2.0),
	p(0.407,  "1400012",  2.0), 
	p(0.329,  "1400013",  3.0), 
	p(0.629,  "1400014",  2.7), 
	p(0.417,  "1400015",  1.2), 
	p(0.278,  "1400016",  1.4), 
	p(0.583,  "1400017",  2.2), 
	p(0.598,  "1400018",  1.9), 
	p(0.271,  "1400019",  1.6), 
	p(0.265,  "1400020",  2.1)],
	[] %La lista inicial de bandejas estará vacía, aun no hay bandejas
	).
  %papayas1().

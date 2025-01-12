# Parsing di stringhe URI

Questo progetto realizza una libreria implementata in **Common Lisp** (un dialetto di **Lisp**) contenente delle strutture che rappresentano internamente delle URI a partire dalla loro rappresentazione come stringhe.

- [Utilizzo](#utilizzo)
- [Funzionamento](#funzionamento)
- [Autori](#autori)
- [Note Aggiuntive](#note-aggiuntive)

## Utilizzo

La libreria utilizza la funzione **urilib-parse**, che restituisce un **uri-struct** che contiene i componenti di un URI scomposto, a partire da una stringa contenente un URI in formato testuale;

Per utilizzare il programma basterà quindi eseguire la funzione **urilib-parse**.

Un esempio di chiamata è:  
**(urilib-parse ”http://disco.unimib.it”)**

L'output ricevuto sarà:  
*#S(URI-STRUCT :SCHEME "http" :USERINFO NIL :HOST "disco.unimib.it" :PORT 80 :PATH NIL :QUERY NIL :FRAGMENT NIL)*


## Funzionamento

La libreria suddivide la stringa dell'URI nei seguenti componenti:
1. Schema
2. Userinfo
3. Host
4. Port
5. Path
6. Query
7. Fragment

Questo è eseguito grazie all'esistenza di una funzione per ogni componente dell'URI presente in uri-struct, in grado di restituire il singolo componente dallo struct:

- **urilib-scheme**
- **urilib-userinfo**
- **urilib-host**
- **urilib-port**
- **urilib-path**
- **urilib-query**
- **urilib-fragment**

---

L'elemento fondamentale di un URI è lo schema, che definisce le regole sintattiche da rispettare. Schemi differenti permettono di identificare tipi diversi di risorse, determinando quali componenti debbano essere presenti e quale struttura specifica debba avere ciascun componente.

Questo parser riconosce diversi schemi per gli URI:

+ Schemi generali
	- http
	- https
	- ftp
+ Schemi speciali
	- mailto
	- news
	- tel
	- fax
	- zos

Questo ci permette prima di riconosciuto lo schema utilizzato, poi di impiegare diverse funzioni, ognuna in grado di eseguire il parse dei singoli componenti.

A seconda che si utilizzi uno schema speciale o generale, si applicano regole sintattiche differenti, che possono portare alla scomposizione della stringa e alla restituzione del risultato, questo perchè non per ogni schema l'URI conterrà tutti i componenti standard sopra indicati, e potrebbero essere contenuti in formati diversi;

Ecco una tabella che indica quali componenti troviamo in un URI di ogni schema:

| Schema     | Componenti |
|----------|-----|
| Generali    | Schema, Userinfo, Host, Port, Path, Query, Fragment |
| Mailto    | Schema, Userinfo, Host |
| News    | Schema, Host |
| Tel    | Schema, Userinfo |
| Fax    | Schema, Userinfo |
| Zos    | Schema, Userinfo, Host, Port, Path, Query, Fragment  |

Nel caso in cui un componente non sia presente, nell' uri-struct di output sarà presente "NIL" nella voce di quel componente (vedi esempio sopra riportato, dove non sono presenti Userinfo, Path, Query e Fragment).

---

La funzione **urilib-display** consente di stampare dati su uno stream di destinazione specificato. 

Qualora venga passato uno stream come argomento, l'output sarà diretto a tale stream;
In assenza di uno stream esplicitamente indicato, l'output verrà prodotto sullo stream corrente.

## Autori
Progetto del corso di Linguaggi e Programmazione, realizzato da:
- Simone Monzardo: *s.monzardo1@campus.unimib.it*
- Gabriele Furlan: *g.furlan7@campus.unimib.it*
- Gabriele Beretta: *g.beretta51@campus.unimib.it*

## Note aggiuntive

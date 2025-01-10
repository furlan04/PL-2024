# Parsing di stringhe URI

Questo progetto realizza una libreria contenente delle strutture che rappresentano internamente delle URI a partire dalla loro rappresentazione come stringhe.

- [Utilizzo](#utilizzo)
- [Funzionamento](#funzionamento)
- [Autori](#autori)
- [Note Aggiuntive](#note-aggiuntive)

## Utilizzo

La libreria utilizza il predicato **urilib_parse/2**, il quale analizza una stringa rappresentante un URI in formato testuale e ne restituisce una versione suddivisa nei suoi componenti.

L'interprete restituisce altrimenti <span style="color: red;">False</span> se la stringa non è in un formato corretto.


Un esempio di chiamata è:  
**?- urilib_parse(”http://disco.unimib.it”, URI).**


Il risultato della query sarà:  
*URI = uri(http, [], ’disco.unimib.it’, 80, [], [], [])*


## Funzionamento

La libreria suddivide la stringa dell'URI nei seguenti componenti:
1. Schema
2. Userinfo
3. Host
4. Port
5. Path
6. Query
7. Fragment

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

Per fare ciò, viene prima riconosciuto lo schema utilizzato tramite il predicato **parse_uri_with_schema/3**,poi vengono impiegati diversi predicati, ognuno in grado di eseguire il parse dei singoli componenti.

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

---

Sono disponibili due predicati, **uri_display/1** e **uri_display/2**, per stampare su uno stream di destinazione.

Nel caso di **uri_display/2**, lo schema deve essere fornito come argomento, mentre **uri_display/1** utilizza lo stream corrente per richiamare **uri_display/2**.

## Autori
Progetto del corso di Linguaggi e Programmazione, realizzato da:
- Simone Monzardo: *s.monzardo1@campus.unimib.it*
- Gabriele Furlan: *g.furlan7@campus.unimib.it*
- Gabriele Beretta: *g.beretta51@campus.unimib.it*

## Note aggiuntive

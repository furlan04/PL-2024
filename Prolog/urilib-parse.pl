urilib_parse(URIString, URI) :-
    string(URIString),
    atom_codes(URIString, Codes),
    schema(Codes, SchemaCodes, AfterSchema),
    atom_codes(Schema, SchemaCodes),
    parse_uri_with_schema(Schema, AfterSchema, URI).

urilib_display(URI) :-
    current_output(Stream),
    urilib_display(URI, Stream).
urilib_display(URI, Stream) :-
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment),
    format(Stream, 'Schema = ~w\n', [Schema]),
    format(Stream, 'Userinfo = ~w\n', [Userinfo]),
    format(Stream, 'Host = ~w\n', [Host]),
    format(Stream, 'Port = ~w\n', [Port]),
    format(Stream, 'Path = ~w\n', [Path]),
    format(Stream, 'Query = ~w\n', [Query]),
    format(Stream, 'Fragment = ~w\n', [Fragment]),
    close(Stream).

%!  parse_uri_with_schema(+Schema, +AfterSchema, -URI)
%

% Schema seguito dal nulla
parse_uri_with_schema(Schema, [], URI) :-
    !,
    URI = uri(Schema, [], [], 80, [], [], []).

% -- FORMATO ZOS --

% ZOS: Path completo con Query e Fragment
parse_uri_with_schema(Schema, [], URI) :-
    !,
    URI = uri(Schema, [], [], 80, [], [], []).

%   Parse di un URI senza Fragment secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    isolate_path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, []).

%   Parse di un URI senza Query secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    isolate_path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], Fragment).

%   Parse di un URI senza Query e Fragment secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    isolate_path(AfterAuthority, PathCodes, []),
    zos_path(PathCodes),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

%   Parse di un URI completo secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    !,
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    isolate_path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).

%   Parse di un URI senza Path secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, Fragment).

%   Parse di un URI senza Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    (PathCodes = [],
        Path = [] ; atom_codes(Path, PathCodes)),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, []).

%   Parse di un URI senza Query secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    (PathCodes = [],
        Path = [] ; atom_codes(Path, PathCodes)),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], Fragment).

%   Parse di un URI senza Query e Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, []),
    !,
    (PathCodes = [],
        Path = [] ; atom_codes(Path, PathCodes)),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

%   Parse di un URI senza Path e Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, []).

%   Parse di un URI senza Path e Query secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], [], Fragment).

%   Parse di un URI senza Path, Query e Fragment secondo il formato generico,
%   terminato dal carattere "/".
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], []),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

%   Parse di un URI senza Path, Query e Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    (AfterAuthority = [];
     AfterAuthority = [47]),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

%   Parse di un URI completo secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).

%   Parse di un URI contenente solo Userinfo secondo il formato dello Schema
%   mailto.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, []),
    !,
    atom_codes(Userinfo, U),
    Userinfo \= '',
    URI = uri('mailto', Userinfo, [], [], [], [], []).

%   Parse di un URI contenente Userinfo e Host secondo il formato dello Schema
%   mailto.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, AfterUserinfo),
    host(AfterUserinfo, H, []),
    !,
    atom_codes(Userinfo, U),
    Userinfo \= '',
    atom_codes(Host, H),
    URI = uri('mailto', Userinfo, Host, [], [], [], []).

%   Parse di un URI contenente solo l'Host secondo il formato dello Schema news.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'news',
    host(AfterSchema, H, []),
    !,
    atom_codes(Host, H),
    URI = uri('news', [], Host, [], [], [], []).

%   Parse di un URI contenente solo Userinfo secondo il formato degli Schema
%   tel e fax.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    (Schema = 'tel';
     Schema = 'fax'),
    plain_userinfo(AfterSchema, U, []),
    !,
    atom_codes(Userinfo, U),
    URI = uri(Schema, Userinfo, [], [], [], [], []).

%!  schema(+Codes, -Schema, -After)
%
%   Lo Schema viene estratto dai codici passati in input.

schema([C | Codes], Schema, After) :-
    C = 58, % :
    !,
    Schema = [],
    After = Codes.
schema([C | Codes], Schema, After) :-
    identificatore(C),
    !,
    schema(Codes, S, After),
    Schema = [C | S].

%!  authority(+Codes, -Userinfo, -Host, -Port, -After)
%
%   I componenti dell'Authority vengono estratti dai codici passati in input.

%   Parse di Authority completa.
%!  authority(+Codes, -Userinfo, -Host, -Port, -After)
%
%   I componenti dell'Authority vengono estratti dai codici passati in input.

%   Parse di Authority completa.

default_port(http, 80).
default_port(https, 443).
default_port(zos, 23).
default_port(news, 119).
default_port(mailto, 25).
default_port(ftp, 21).
default_port(_, 80).

authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    userinfo(Chars, U, AfterUserinfo),
    (ip(AfterUserinfo, H, AfterHost);
     host(AfterUserinfo, H, AfterHost)),
    port(AfterHost, P, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    Host \= '',
    number_codes(Port, P).

%   Parse di Authority senza Userinfo.
authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    (ip(Chars, H, AfterHost);
     host(Chars, H, AfterHost)),
    port(AfterHost, P, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    Host \= '',
    number_codes(Port, P).

%   Parse di Authority senza Port, sostituita quindi da quella di default.
authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    userinfo(Chars, U, AfterUserinfo),
    (ip(AfterUserinfo, H, After);
     host(AfterUserinfo, H, After)),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    Host \= '',
    default_port(Schema, Port).

%   Parse di Authority senza Userinfo e Port.
authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    (ip(Chars, H, After);
     host(Chars, H, After)),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    Host \= '',
    default_port(Schema, Port).

%   Parse di Authority non presente, quindi si passa direttamente al Path.
authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47,  
    C2 \= 47, 
    !,
    Userinfo = [],
    Host = [],
    default_port(Schema, Port),
    After = [C1, C2 | Chars].

authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 35, 
    C2 \= 35, 
    !,
    Userinfo = [],
    Host = [],
    default_port(Schema, Port),
    After = [C1, C2 | Chars].

authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 63, 
    C2 \= 63,
    !,
    Userinfo = [],
    Host = [],
    default_port(Schema, Port),
    After = [C1, C2 | Chars].

%!  userinfo(+Codes, -Userinfo, -After)
%
%   Lo Userinfo viene estratto dai codici passati in input.

userinfo([C | Codes], Userinfo, After) :-
    C = 64, % @
    !,
    Userinfo = [],
    After = Codes.
userinfo([C | Codes], Userinfo, After) :-
    identificatore(C),
    !,
    userinfo(Codes, U, After),
    Userinfo = [C | U].

%!  plain_userinfo(+Codes, -Userinfo, -After)
%
%   Lo Userinfo viene estratto dai codici passati in input.

plain_userinfo([], Userinfo, After) :-
    Userinfo = [],
    After = [].
plain_userinfo([C | Codes], Userinfo, After) :-
    C = 64, % @
    !,
    Userinfo = [],
    After = Codes.
plain_userinfo([C | Codes], Userinfo, After) :-
    identificatore(C),
    !,
    plain_userinfo(Codes, U, After),
    Userinfo = [C | U].

%!  host(+Codes, -Host, -After)
%
%   L'Host viene estratto dai codici passati in input.

host([], Host, After) :-
    Host = [],
    After = [].

host([C | Codes], Host, After) :-
    (C = 58;  % :
     C = 47;  % /
     C = 63;  % ?
     C = 35), % #
    !,
    Host = [],
    After = [C | Codes].

% Verifichiamo che l'host inizi con una lettera
host([C | Codes], Host, After) :-
    is_alpha(C),  % deve iniziare con una lettera
    !,
    host_aux(Codes, C, RestHost, After),
    Host = [C | RestHost].

host_aux([], _, Host, After) :-
    Host = [],
    After = [].

host_aux([C | Codes], _, Host, After) :-
    (C = 58;  % :
     C = 47;  % /
     C = 63;  % ?
     C = 35), % #
    !,
    Host = [],
    After = [C | Codes].

host_aux([46 | Codes], PrevChar, Host, After) :-  % caso del punto
    PrevChar \= 46,  % verifica che il carattere precedente non sia un punto
    Codes = [C2 | Rest],  % prendi il carattere dopo il punto
    is_alpha(C2),  % deve essere una lettera
    !,
    host_aux(Rest, C2, RestHost, After),
    Host = [46, C2 | RestHost].

host_aux([C | Codes], _, Host, After) :-
    C \= 46,  % non punto
    identificatore(C),
    !,
    host_aux(Codes, C, RestHost, After),
    Host = [C | RestHost].

ip(Codes, Host, After):-
    octet(Codes, FirstOctet, [46 | AfterFirstOctet]),
    octet(AfterFirstOctet, SecondOctet, [46 | AfterSecondOctet]),
    octet(AfterSecondOctet, ThirdOctet, [46 | AfterThirdOctet]),
    octet(AfterThirdOctet, FourthOctet, After),
    append(FirstOctet, [46], FirstAppend),
    append(SecondOctet, [46], SecondAppend),
    append(ThirdOctet, [46], ThirdAppend),
    append(FirstAppend, SecondAppend, FourthAppend),
    append(FourthAppend, ThirdAppend, FifthAppend),
    append(FifthAppend, FourthOctet, Host).

%!  octet(+Codes, -Octet, -After)
%
%   Vengono estratti e convertiti gli otto bit che compongono un ottetto
%   dai codici passati in input.

%   Ottetto composto da tre cifre.
octet([A, B, C | After], Octet, After):-
    is_digit(A),
    is_digit(B),
    is_digit(C),
    number_codes(Number, [A, B, C]),
    Number =< 255,
    Number >= 0,
    Octet = [A, B, C],
    !.

%   Ottetto composto da due cifre.
octet([A, B | After], Octet, After):-
    is_digit(A),
    is_digit(B),
    number_codes(Number, [A, B]),
    Number =< 255,
    Number >= 0,
    Octet = [A, B],
    !.

%   Ottetto composto da una cifra.
octet([A | After], Octet, After):-
    is_digit(A),
    number_codes(Number, [A]),
    Number =< 255,
    Number >= 0,
    Octet = [A],
    !.

%!  port(+Codes, -Port, -After)
%
%   Viene estratta Port dai codici passati in input.

% Port must contain only digits and return false for invalid input
port([58 | Codes], Port, After) :-  % :
    port_aux(Codes, Port, After),
    Port \= [],
    number_codes(_, Port).

port_aux([], [], []).
port_aux([47 | Rest], [], [47 | Rest]) :- !.  
port_aux([63 | Rest], [], [63 | Rest]) :- !.  
port_aux([35 | Rest], [], [35 | Rest]) :- !.  
port_aux([C | Codes], [C | Port], After) :-
    is_digit(C),
    port_aux(Codes, Port, After).

path([47 | Codes], Path, After) :-  % /
    path_aux(Codes, Path, After).

path_aux([], [], []).
path_aux([63 | Rest], [], [63 | Rest]) :- !.  % ?
path_aux([35 | Rest], [], [35 | Rest]) :- !.  % #
path_aux([47, C | Codes], [47, C | Path], After) :-
    identificatore(C), 
    path_aux(Codes, Path, After). 
path_aux([C | Codes], [C | Path], After) :-
    identificatore(C),  % This will fail if C is not a valid character
    path_aux(Codes, Path, After).
%
%   Viene estratto il Path dai codici passati in input secondo il formato
%   dello Schema zos.

%   Parse del Path contenente solo Id44 secondo il formato dello Schema zos.

isolate_path([47 | Codes], Path, After) :-  % /
    isolate_path_aux(Codes, Path, After).

isolate_path_aux([], [], []).
isolate_path_aux([63 | Rest], [], [63 | Rest]) :- !.  % ?
isolate_path_aux([35 | Rest], [], [35 | Rest]) :- !.  % #.
isolate_path_aux([C | Codes], [C | Path], After) :-
    isolate_path_aux(Codes, Path, After).

zos_path([C | Codes]) :-
    is_alpha(C),
    id44([C | Codes], Id44Codes, []),
    !,
    length(Id44Codes, Id44Length),
    Id44Length =< 44,
    last(Id44Codes, LastId44Character),
    LastId44Character \= 46.

%   Parse del Path contenente Id44 e Id8 secondo il formato dello Schema zos.
zos_path([C | Codes]) :-
    is_alpha(C),
    id44([C | Codes], Id44Codes, AfterId44Codes),
    length(Id44Codes, Id44Length),
    Id44Length =< 44,
    last(Id44Codes, LastId44Character),
    LastId44Character \= 46,
    id8(AfterId44Codes, Id8Codes),
    length(Id8Codes, Id8Length),
    Id8Length =< 8.

%!  id44(+Codes, -Id44, -After)
%
%   Viene estratto l'Id44 dai codici passati in input che compongono un Path,
%   secondo il formato dello Schema zos.

id44([], Id44, After) :-
    Id44 = [],
    After = [].
id44([C | Codes], Id44, After) :-
    C = 40, % (
    !,
    Id44 = [],
    After = [C | Codes].
id44([C | Codes], Id44, After) :-
    (is_alnum(C);
     C = 46), % .
    !,
    id44(Codes, I, After),
    Id44 = [C | I].

%!  id8(+Codes, -Id44, -After)
%
%   Viene estratto l'Id8 dai codici passati in input che compongono un Path,
%   secondo il formato dello Schema zos.

parse_id8([C | []], Id8) :-
    C = 41, % )
    !,
    Id8 = [].
parse_id8([C | Codes], Id8) :-
    is_alnum(C),
    !,
    parse_id8(Codes, I),
    Id8 = [C | I].
id8([C1, C2 | Codes], Id8) :-
    C1 = 40, % (
    is_alpha(C2),
    !,
    parse_id8([C2 | Codes], Id8).

%!  query(+Codes, -Query, -After)
%
%   Viene estratto il componente Query dai codici passati in input.

parse_query([], Query, After) :-
    Query = [],
    After = [].
parse_query([C | Codes], Query, After) :-
    C = 35, % #
    !,
    Query = [],
    After = [C | Codes].
parse_query([C | Codes], Query, After) :-
    caratteri(C),
    !,
    parse_query(Codes, Q, After),
    Query = [C | Q].
query([C | Codes], Query, After) :-
    C = 63, % ?
    !,
    parse_query(Codes, Query, After).

parse_fragment([], Fragment, After) :-
    Fragment = [],
    After = [].
parse_fragment([C | Codes], Fragment, After) :-
    caratteri(C),
    !,
    parse_fragment(Codes, F, After),
    Fragment = [C | F].
fragment([C | Codes], Fragment, After) :-
    C = 35, % #
    !,
    parse_fragment(Codes, Fragment, After).

caratteri(C) :-
    is_alnum(C);
    C = 45;  % -
    C = 95;  % _
    C = 43;  % +
    C = 61.  % =

identificatore(C) :-
    is_alnum(C);
    C = 45;  % -
    C = 95;  % _
    C = 43;  % +
    C = 61.  % =

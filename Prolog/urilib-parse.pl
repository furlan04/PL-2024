urilib_parse(URIString, URI) :-
    string(URIString),
    atom_codes(URIString, Codes),
    schema(Codes, SchemaCodes, AfterSchema),
    atom_codes(Schema, SchemaCodes),
    parse_uri_with_schema(Schema, AfterSchema, URI).

urilib_display(URI) :-
    current_output(Stream),
    uri_display(URI, Stream).
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
%   A partire dallo Schema passato in input, tramite l'applicazione delle
%   regole sintattiche associate vengono estratti i componenti dell'URI
%   mancanti da ciò che si trova dopo lo Schema, passato come AfterSchema.

%   Uno Schema seguito dal nulla è un URI valido.
%!  parse_uri_with_schema(+Schema, +AfterSchema, -URI)
%
%   A partire dallo Schema passato in input, tramite l'applicazione delle
%   regole sintattiche associate vengono estratti i componenti dell'URI
%   mancanti da ciò che si trova dopo lo Schema, passato come AfterSchema.

% Schema seguito dal nulla è un URI valido
parse_uri_with_schema(Schema, [], URI) :-
    !,
    URI = uri(Schema, [], [], 80, [], [], []).

% -- FORMATO ZOS --

% ZOS: Path completo con Query e Fragment
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
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
    path(AfterAuthority, PathCodes, AfterPath),
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
    path(AfterAuthority, PathCodes, []),
    zos_path(PathCodes),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

%   Parse di un URI completo secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    !,
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).

% -- FORMATO GENERICO --

% Caso 1: URI completo con path, query e fragment
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

% Caso 2: URI con path e query (no fragment)
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, []).

% Caso 3: URI con path e fragment (no query)
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], Fragment).

% Caso 4: URI con solo path
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, []),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

% Caso 5: URI senza path ma con query e fragment
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    AfterAuthority = [63 | _],  % inizia con ?
    query(AfterAuthority, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, Fragment).

% Caso 6: URI senza path ma con query
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    AfterAuthority = [63 | _],  % inizia con ?
    query(AfterAuthority, QueryCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, []).

% Caso 7: URI senza path ma con fragment
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    AfterAuthority = [35 | _],  % inizia con #
    fragment(AfterAuthority, FragmentCodes, []),
    !,
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], [], Fragment).

% Caso 8: URI con solo authority
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    authority(Schema, AfterSchema, Userinfo, Host, Port, []),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

% -- FORMATO MAILTO --

% Mailto: Solo userinfo
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, []),
    !,
    atom_codes(Userinfo, U),
    Userinfo \= '',
    URI = uri('mailto', Userinfo, [], [], [], [], []).

% Mailto: Userinfo e host
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, AfterUserinfo),
    host(AfterUserinfo, H, []),
    !,
    atom_codes(Userinfo, U),
    Userinfo \= '',
    atom_codes(Host, H),
    URI = uri('mailto', Userinfo, Host, [], [], [], []).

% -- FORMATO NEWS --

% News: Solo host
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'news',
    host(AfterSchema, H, []),
    !,
    atom_codes(Host, H),
    URI = uri('news', [], Host, [], [], [], []).

% -- FORMATO TEL/FAX --

% Tel/Fax: Solo userinfo
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

%   Parse di Authority senza Userinfo e Port. La Port viene sostituita quindi
%   con quella di default. Questo caso gestisce anche quando dopo l'host c'è
%   direttamente # o ?
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
%   Lo Userinfo viene estratto dai codici passati in input. In questo caso
%   lo Userinfo può essere l'unico componente presente all'interno di un URI.

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
host([C | Codes], Host, After) :-
    identificatore(C),
    !,
    host(Codes, P, After),
    Host = [C | P].

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

parse_port([], Port, After) :-
    Port = [],
    After = [].
parse_port([C | Codes], Port, After) :-
    C = 47, % /
    !,
    Port = [],
    After = [C | Codes].
parse_port([C | Codes], Port, After) :-
    is_digit(C),
    !,
    parse_port(Codes, P, After),
    Port = [C | P].
port([C | Codes], Port, After) :-
    C = 58, % :
    !,
    parse_port(Codes, Port, After),
    Port \= [].

%!  path(+Codes, -Path, -After)
%
%   Viene estratto il Path dai codici passati in input.

parse_path([], Path, After) :-
    Path = [],
    After = [].
parse_path([C | Codes], Path, After) :-
    (C = 63;  % ?
     C = 35), % #
    !,
    Path = [],
    After = [C | Codes].
parse_path([C | Codes], Path, After) :-
    (identificatore(C);
     C = 47), % /
    !,
    parse_path(Codes, P, After),
    Path = [C | P].
path([C | Codes], Path, After) :-
    C = 47, % /
    !,
    parse_path(Codes, Path, After).

%!  zos_path(+Codes)
%
%   Viene estratto il Path dai codici passati in input secondo il formato
%   dello Schema zos.

%   Parse del Path contenente solo Id44 secondo il formato dello Schema zos.
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
    C = 32;  % Space
    C = 45;  % -
    C = 46;  % .
    C = 95;  % _
    C = 126; % ~
    C = 58;  % :
    C = 47;  % /
    C = 63;  % ?
    C = 35;  % #
    C = 91;  % [
    C = 93;  % ]
    C = 64;  % @
    C = 33;  % !
    C = 36;  % $
    C = 38;  % &
    C = 39;  % '
    C = 40;  % (
    C = 41;  % )
    C = 42;  % *
    C = 43;  % +
    C = 44;  % ,
    C = 59;  % ;
    C = 61.  % =

identificatore(C) :-
    is_alnum(C);
    C = 32;  % Space
    C = 45;  % -
    C = 46;  % .
    C = 95;  % _
    C = 126; % ~
    C = 91;  % [
    C = 93;  % ]
    C = 33;  % !
    C = 36;  % $
    C = 38;  % &
    C = 39;  % '
    C = 40;  % (
    C = 41;  % )
    C = 42;  % *
    C = 43;  % +
    C = 44;  % ,
    C = 59;  % ;
    C = 61.  % =

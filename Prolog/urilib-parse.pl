% predicate urilib_parse/2
urilib_parse(URIString, URI) :-
    string(URIString),
    atom_codes(URIString, Codes),
    schema(Codes, SchemaCodes, AfterSchema),
    atom_codes(Schema, SchemaCodes),
    parse_uri_with_schema(Schema, AfterSchema, URI).

% predicate urilib_display/1 e urilib_display/2
% prints the URI components to the current output stream
% or to the specified stream
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

% predicate parse_uri_with_schema/3
%  Schema: URI schema
%  AfterSchema: remaining characters after the schema
%  URI: parsed URI 

parse_uri_with_schema(Schema, [], URI) :-
    generic_scheme(Schema),
    default_port(Schema, Port),
    !,
    URI = uri(Schema, [], [], Port, [], [], []).

% -- ZOS FORMAT --
% authority is handled like in the generic format
% path parsed as required by the zos format

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

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    isolate_path(AfterAuthority, PathCodes, []),
    zos_path(PathCodes),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    isolate_path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    isolate_path_aux(AfterSchema, PathCodes, []),
    zos_path(PathCodes),
    default_port(Schema, Port),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, [], [], Port, Path, [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    isolate_path_aux(AfterSchema, PathCodes, AfterPath),
    zos_path(PathCodes),
    default_port(Schema, Port),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, [], [], Port, Path, Query, Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    isolate_path_aux(AfterSchema, PathCodes, AfterPath),
    zos_path(PathCodes),
    default_port(Schema, Port),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, [], [], Port, Path, Query, []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    isolate_path_aux(AfterSchema, PathCodes, AfterPath),
    zos_path(PathCodes),
    default_port(Schema, Port),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, [], [], Port, Path, [], Fragment).

% -- GENERIC FORMAT --
% authority is handled like in the generic format
% path parsed as required by the generic format

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    path_codes_empty(PathCodes, Path),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    path_codes_empty(PathCodes, Path),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, []),
    !,
    path_codes_empty(PathCodes, Path),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], [], Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], []),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    check_last(AfterAuthority),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    path(AfterSchema, PathCodes, []),
    default_port(Schema, Port),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, [], [], Port, Path, [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    path_aux(AfterSchema, PathCodes, []),
    default_port(Schema, Port),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, [], [], Port, Path, [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    path_aux(AfterSchema, PathCodes, AfterPath),
    default_port(Schema, Port),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    path_codes_empty(PathCodes, Path),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, [], [], Port, Path, Query, Fragment).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    path_aux(AfterSchema, PathCodes, AfterPath),
    default_port(Schema, Port),
    query(AfterPath, QueryCodes, []),
    !,
    path_codes_empty(PathCodes, Path),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, [], [], Port, Path, Query, []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    generic_scheme(Schema),
    path_aux(AfterSchema, PathCodes, AfterPath),
    default_port(Schema, Port),
    fragment(AfterPath, FragmentCodes, []),
    !,
    path_codes_empty(PathCodes, Path),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, [], [], Port, Path, [], Fragment).

% -- MAILTO FORMAT --
% mandatory Userinfo
% Host is optional

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, []),
    !,
    atom_codes(Userinfo, U),
    Userinfo \= '',
    URI = uri('mailto', Userinfo, [], [], [], [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, AfterUserinfo),
    host(AfterUserinfo, H, []),
    !,
    atom_codes(Userinfo, U),
    Userinfo \= '',
    atom_codes(Host, H),
    URI = uri('mailto', Userinfo, Host, [], [], [], []).

% -- NEWS FORMAT --
% mandatory Host
% no other components

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'news',
    host(AfterSchema, H, []),
    !,
    atom_codes(Host, H),
    URI = uri('news', [], Host, [], [], [], []).

% -- TEL AND FAX FORMAT --
% mandatory Userinfo
% no other component

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    tel_or_fax(Schema),
    plain_userinfo(AfterSchema, U, []),
    !,
    atom_codes(Userinfo, U),
    URI = uri(Schema, Userinfo, [], [], [], [], []).

% PARSING of the separate URI components

% Schema parsing

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

% default port handling, based on the schema

default_port(http, 80).
default_port(https, 443).
default_port(zos, 23).
default_port(news, 119).
default_port(mailto, 25).
default_port(ftp, 21).
default_port(_, 80).

% Authority parsing

% check if the input is a valid IP address
ip_or_host(Chars, H, AfterHost) :-
    ip(Chars, H, AfterHost).
% check if the input is a valid host
ip_or_host(Chars, H, AfterHost) :-
    host(Chars, H, AfterHost).

authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    userinfo(Chars, U, AfterUserinfo),
    ip_or_host(AfterUserinfo, H, AfterHost),
    port(AfterHost, P, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    Host \= '',
    number_codes(Port, P).

authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    ip_or_host(Chars, H, AfterHost),
    port(AfterHost, P, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    Host \= '',
    number_codes(Port, P).

authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    userinfo(Chars, U, AfterUserinfo),
    ip_or_host(AfterUserinfo, H, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    Host \= '',
    default_port(Schema, Port).

authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % /
    C2 = 47, % /
    ip_or_host(Chars, H, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    Host \= '',
    default_port(Schema, Port).

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

% parsing Userinfo

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

% parsing Host

host([], Host, After) :-
    Host = [],
    After = [].

host([C | Codes], Host, After) :-
    special_char(C),
    !,
    Host = [],
    After = [C | Codes].

host([C | Codes], Host, After) :-
    is_alpha(C),
    !,
    host_aux(Codes, C, RestHost, After),
    Host = [C | RestHost].

host_aux([], _, Host, After) :-
    Host = [],
    After = [].

host_aux([C | Codes], _, Host, After) :-
    special_char(C),
    !,
    Host = [],
    After = [C | Codes].

% check if . are followed by a letter
host_aux([46 | Codes], PrevChar, Host, After) :-  
    PrevChar \= 46,  
    Codes = [C2 | Rest],  
    is_alpha(C2), 
    !,
    host_aux(Rest, C2, RestHost, After),
    Host = [46, C2 | RestHost].

host_aux([C | Codes], _, Host, After) :-
    C \= 46,
    is_alnum(C),
    !,
    host_aux(Codes, C, RestHost, After),
    Host = [C | RestHost].

% parsing IP

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

octet([A, B, C | After], Octet, After):-
    is_digit(A),
    is_digit(B),
    is_digit(C),
    number_codes(Number, [A, B, C]),
    Number =< 255,
    Number >= 0,
    Octet = [A, B, C],
    !.

octet([A, B | After], Octet, After):-
    is_digit(A),
    is_digit(B),
    number_codes(Number, [A, B]),
    Number =< 255,
    Number >= 0,
    Octet = [A, B],
    !.

octet([A | After], Octet, After):-
    is_digit(A),
    number_codes(Number, [A]),
    Number =< 255,
    Number >= 0,
    Octet = [A],
    !.

% parsing Port

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

% parsing Path

path([47, C | Codes], Path, After) :-  % /
    path_accettati_secondo_char(C),
    path_aux([C | Codes], Path, After).

path_aux([], [], []).
path_aux([63 | Rest], [], [63 | Rest]) :- !.  % ?
path_aux([35 | Rest], [], [35 | Rest]) :- !.  % #
path_aux([47, C | Codes], [47, C | Path], After) :-
    identificatore(C),
    path_aux(Codes, Path, After).
path_aux([C | Codes], [C | Path], After) :-
    identificatore(C),
    path_aux(Codes, Path, After).

isolate_path([47 | Codes], Path, After) :-  % /
    isolate_path_aux(Codes, Path, After).

isolate_path_aux([], [], []).
isolate_path_aux([63 | Rest], [], [63 | Rest]) :- !.  % ?
isolate_path_aux([35 | Rest], [], [35 | Rest]) :- !.  % #.
isolate_path_aux([C | Codes], [C | Path], After) :-
    isolate_path_aux(Codes, Path, After).

% parsing Path secondo lo Schema zos

zos_path([C | Codes]) :-
    is_alpha(C),
    id44([C | Codes], Id44Codes, []),
    !,
    length(Id44Codes, Id44Length),
    Id44Length =< 44,
    last(Id44Codes, LastId44Character),
    LastId44Character \= 46.

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

% checks if the input is a valid id44

id44([], Id44, After) :-
    Id44 = [],
    After = [].
id44([C | Codes], Id44, After) :-
    C = 40, % (
    !,
    Id44 = [],
    After = [C | Codes].
id44([C | Codes], Id44, After) :-
    is_alnum(C),
    !,
    id44(Codes, I, After),
    Id44 = [C | I].
id44([C | Codes], Id44, After) :-
    C = 46, % .
    !,
    id44(Codes, I, After),
    Id44 = [C | I].

% checks if the input is a valid id8

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

% parsing Query

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

% UTILITY PREDICATES

caratteri(C) :-
    is_alnum(C).
caratteri(C) :-
    C = 45.  % -
caratteri(C) :-
    C = 95.  % _
caratteri(C) :-
    C = 43.  % +
caratteri(C) :-
    C = 61.  % =

identificatore(C) :-
    is_alnum(C).
identificatore(C) :-
    C = 45.  % -
identificatore(C) :-
    C = 95.  % _
identificatore(C) :-
    C = 43.  % +
identificatore(C) :-
    C = 61.  % =

generic_scheme(Schema) :-
    Schema \= 'zos',
    Schema \= 'mailto',
    Schema \= 'tel',
    Schema \= 'fax',
    Schema \= 'news'.

path_codes_empty(PathCodes, Path) :-
    PathCodes = [],        
    Path = []. 
path_codes_empty(PathCodes, Path) :-
    atom_codes(Path, PathCodes).

tel_or_fax('tel').
tel_or_fax('fax').

check_last([]).
check_last([47]).

special_char(C) :-
    C = 58.
special_char(C) :-
    C = 47.
special_char(C) :-
    C = 63.
special_char(C) :-
    C = 35.

path_accettati_secondo_char(C) :-
    identificatore(C).
path_accettati_secondo_char(C) :-
    C = 63.
path_accettati_secondo_char(C) :-
    C = 35.
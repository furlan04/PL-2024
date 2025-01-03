% Main URI Parser
urilib_parse(URIString, URI) :-
    atom_string(URIString, String),
    string_codes(String, Chars),
    parse_scheme(Chars, [], SchemeChars, Rest),
    atom_chars(Scheme, SchemeChars),
    scheme_type(Scheme, Type),
    parse_by_type(Type, Scheme, Rest, URI).

% Character Validation Rules
valid_scheme_char(Code) :-
    char_type(Code, alpha), !.
valid_scheme_char(Code) :-
    char_type(Code, digit), !.
valid_scheme_char(Code) :-
    Code = 95.  % underscore

valid_identifier_char(Code) :-
    char_type(Code, alpha), !.
valid_identifier_char(Code) :-
    char_type(Code, digit), !.
valid_identifier_char(Code) :-
    Code = 95.  % underscore

valid_host_identifier_char(Code) :-
    char_type(Code, alpha), !.
valid_host_identifier_char(Code) :-
    char_type(Code, digit), !.
valid_host_identifier_char(Code) :-
    Code = 95, !.  % underscore
valid_host_identifier_char(Code) :-
    Code = 45, !.  % hyphen
valid_host_identifier_char(Code) :-
    Code = 46.     % dot

% Scheme Type Classification
scheme_type(zos, special).
scheme_type(mailto, special).
scheme_type(news, special).
scheme_type(tel, special).
scheme_type(fax, special).
scheme_type(_, generic).

% Parse based on scheme type
parse_by_type(special, Scheme, Rest, URI) :-
    special_scheme_parser(Scheme, Parser),
    call(Parser, Rest, URI).
parse_by_type(generic, Scheme, Rest, URI) :-
    parse_generic_uri(Scheme, Rest, URI).

% Special Scheme Parsers Registry
special_scheme_parser(zos, parse_zos_uri).
special_scheme_parser(mailto, parse_mailto_uri).
special_scheme_parser(news, parse_news_uri).
special_scheme_parser(tel, parse_tel_uri).
special_scheme_parser(fax, parse_fax_uri).

% Scheme Parser
parse_scheme([], _, _, _) :- fail.
parse_scheme(Chars, [], Scheme, Remainder) :-
    parse_scheme_chars(Chars, [], SchemeTemp, Remainder),
    validate_scheme_chars(SchemeTemp),
    Scheme = SchemeTemp.

parse_scheme_chars([Char|Rest], Acc, Scheme, Remainder) :-
    (Char = 58 -> % colon found
        Scheme = Acc,
        Remainder = Rest
    ;
        append(Acc, [Char], NewAcc),
        parse_scheme_chars(Rest, NewAcc, Scheme, Remainder)
    ).

validate_scheme_chars([First|Rest]) :-
    char_type(First, alpha),
    maplist(valid_scheme_char, Rest).

% Generic URI Parser
% Generic URI Parser with Path Fix
parse_generic_uri(Scheme, Rest, uri(Scheme, UserInfo, Host, Port, PathSegments, Query, Fragment)) :-
    parse_authority(Rest, Scheme, Host, UserInfo, Port, PathCodes),
    % Convert PathCodes to proper format if it's an atom
    (atom(PathCodes) -> 
        atom_codes(PathCodes, PathChars)
    ;
        PathChars = PathCodes
    ),
    % Ensure path starts with /
    (PathChars = [47|_] -> 
        FinalPathCodes = PathChars
    ;
        FinalPathCodes = [47|PathChars]
    ),
    parse_path_query_fragment(FinalPathCodes, PathSegments, Query, Fragment),
    validate_port(Port, Scheme).

% Authority Parser with Path Codes Fix
parse_authority([47, 47|Authority], Scheme, Host, UserInfo, Port, Path) :-
    parse_authority_content(Authority, Scheme, Host, UserInfo, Port, Path), !.
parse_authority(Path, _, '', [], 0, Path).

parse_authority_content(Authority, Scheme, Host, UserInfo, Port, Path) :-
    (append(UserInfoChars, [64|HostPortPath], Authority) ->
        parse_userinfo(UserInfoChars, [], UserInfo),
        parse_host_port_path(HostPortPath, Scheme, Host, Port, Path)
    ;
        parse_host_port_path(Authority, Scheme, Host, Port, Path),
        UserInfo = []
    ).

parse_host_port_path(Input, Scheme, Host, Port, Path) :-
    (append(HostPort, [47|PathChars], Input) ->
        Path = [47|PathChars],
        parse_host_port(HostPort, Scheme, Host, Port)
    ;
        parse_host_port(Input, Scheme, Host, Port),
        Path = [47]
    ).

% Special URI Parsers
parse_zos_uri(Rest, uri(zos, [], '', 0, PathSegments, [], [])) :-
    parse_zos_path(Rest, PathSegments).

parse_mailto_uri(Rest, uri(mailto, UserInfo, Host, 25, [], [], [])) :-
    parse_mailto_parts(Rest, UserInfo, Host).

parse_news_uri(Rest, uri(news, [], Host, 0, [], [], [])) :-
    atom_codes(Host, Rest),
    validate_host(Host).

parse_tel_uri(Rest, uri(tel, [UserInfo], '', 0, [], [], [])) :-
    parse_userinfo_tel(Rest, [], [UserInfo]).

parse_fax_uri(Rest, uri(fax, [UserInfo], '', 0, [], [], [])) :-
    parse_userinfo_tel(Rest, [], [UserInfo]).

parse_host_port(Input, Scheme, Host, Port) :-
    (append(HostChars, [58|PortChars], Input) ->
        atom_codes(Host, HostChars),
        number_codes(Port, PortChars)
    ;
        atom_codes(Host, Input),
        default_port(Scheme, Port)
    ),
    validate_host(Host).

% Port Validation
validate_port(0, Scheme) :- !,
    default_port(Scheme, Port),
    Port = Port.
validate_port(Port, _) :-
    integer(Port),
    Port > 0,
    Port < 65536.

% Default Ports
default_port(http, 80).
default_port(https, 443).
default_port(ftp, 21).
default_port(_, 80).

% Host Validation
validate_host(Host) :-
    atom_codes(Host, Codes),
    (validate_ip_address(Codes) ; validate_domain_name(Codes)).

validate_domain_name(Codes) :-
    split_host_segments(Codes, Segments),
    maplist(validate_host_identifier, Segments),
    \+ (append(_, [46,46|_], Codes)).  % No consecutive dots

validate_host_identifier([First|Rest]) :-
    char_type(First, alpha),
    maplist(valid_host_identifier_char, Rest).

split_host_segments(Codes, Segments) :-
    split_by_dot(Codes, [], [], Segments).

split_by_dot([], CurrAcc, Acc, FinalSegments) :-
    (CurrAcc = [] -> 
        reverse(Acc, FinalSegments)
    ;
        reverse([CurrAcc|Acc], FinalSegments)
    ).
split_by_dot([46|Rest], CurrAcc, Acc, Segments) :- % dot
    CurrAcc \= [],
    split_by_dot(Rest, [], [CurrAcc|Acc], Segments).
split_by_dot([Code|Rest], CurrAcc, Acc, Segments) :-
    append(CurrAcc, [Code], NewAcc),
    split_by_dot(Rest, NewAcc, Acc, Segments).

validate_ip_address(Codes) :-
    split_ip_segments(Codes, [], Segments),
    length(Segments, 4),
    maplist(validate_ip_segment, Segments).

% IP Address Handling
split_ip_segments([], CurrAcc, [LastSegment]) :-
    CurrAcc \= [],
    reverse(CurrAcc, NumCodes),
    number_codes(LastSegment, NumCodes).
split_ip_segments([46|Rest], CurrAcc, [Segment|Segments]) :- % dot
    CurrAcc \= [],
    reverse(CurrAcc, NumCodes),
    number_codes(Segment, NumCodes),
    split_ip_segments(Rest, [], Segments).
split_ip_segments([Digit|Rest], CurrAcc, Segments) :-
    char_type(Digit, digit),
    split_ip_segments(Rest, [Digit|CurrAcc], Segments).

validate_ip_segment(Segment) :-
    integer(Segment),
    Segment >= 0,
    Segment =< 255.

% Domain Name Validation
validate_domain_chars([]).
validate_domain_chars([Char|Rest]) :-
    (char_type(Char, alnum) ; Char = 45 ; Char = 46), % alphanumeric, hyphen, dot
    validate_domain_chars(Rest).

% User Info Parser
parse_userinfo([], Acc, [UserInfo]) :-
    validate_userinfo_chars(Acc),
    atom_codes(UserInfo, Acc).
parse_userinfo([Char|Rest], Acc, UserInfo) :-
    valid_identifier_char(Char),
    append(Acc, [Char], NewAcc),
    parse_userinfo(Rest, NewAcc, UserInfo).

validate_userinfo_chars(Chars) :-
    maplist(valid_identifier_char, Chars).

% Tel/Fax User Info Parser
parse_userinfo_tel([], Acc, [UserInfo]) :-
    atom_codes(UserInfo, Acc).
parse_userinfo_tel([Char|Rest], Acc, UserInfo) :-
    append(Acc, [Char], NewAcc),
    parse_userinfo_tel(Rest, NewAcc, UserInfo).

% Mailto Parser
parse_mailto_parts(Chars, UserInfo, Host) :-
    (append(BeforeAt, [64|HostPart], Chars) ->
        atom_codes(Host, HostPart),
        validate_host(Host),
        parse_userinfo(BeforeAt, [], UserInfo)
    ;
        parse_userinfo(Chars, [], UserInfo),
        Host = ''
    ).

% Path, Query and Fragment Parser
parse_path_query_fragment(PathCodes, PathSegments, Query, Fragment) :-
    split_path_query_fragment(PathCodes, PathOnly, QueryCodes, FragmentCodes),
    decode_path_segments(PathOnly, PathSegments),
    decode_query(QueryCodes, Query),
    decode_fragment(FragmentCodes, Fragment).

split_path_query_fragment(Codes, PathCodes, QueryCodes, FragmentCodes) :-
    split_at(Codes, 35, BeforeHash, FragmentCodes),  % split at #
    split_at(BeforeHash, 63, PathCodes, QueryCodes). % split at ?

split_at(Codes, Char, Before, After) :-
    (append(Before, [Char|After], Codes) -> true
    ; Before = Codes, After = []).

% Path Segments Parser
decode_path_segments(PathCodes, Segments) :-
    (PathCodes = [] -> 
        Segments = []
    ; PathCodes = [47|Rest] ->  % starts with /
        (Rest = [] -> 
            Segments = []
        ;
            split_segments(Rest, [], [], ReversedSegments),
            reverse(ReversedSegments, RawSegments),
            validate_path_segments(RawSegments),
            maplist(atom_codes, Segments, RawSegments)
        )
    ;
        split_segments(PathCodes, [], [], ReversedSegments),
        reverse(ReversedSegments, RawSegments),
        validate_path_segments(RawSegments),
        maplist(atom_codes, Segments, RawSegments)
    ).

% Validate each segment is non-empty and contains only valid characters
validate_path_segments([]).
validate_path_segments([Segment|Rest]) :-
    Segment \= [],  % reject empty segments
    maplist(valid_path_char, Segment),
    validate_path_segments(Rest).

% Valid path characters (excluding special characters)
valid_path_char(Code) :-
    char_type(Code, alnum), !;  % letters and numbers
    Code = 45, !;  % hyphen
    Code = 95.     % underscore

split_segments([], [], Acc, Acc).
split_segments([], CurrAcc, Acc, [Segment|Acc]) :-
    decode_percent_encoded(CurrAcc, DecodedCodes),
    DecodedCodes \= [],  % reject empty segments
    maplist(valid_path_char, DecodedCodes),
    Segment = DecodedCodes.
split_segments([47|Rest], CurrAcc, Acc, Segments) :- % /
    decode_percent_encoded(CurrAcc, DecodedCodes),
    DecodedCodes \= [],  % reject empty segments
    maplist(valid_path_char, DecodedCodes),
    split_segments(Rest, [], [DecodedCodes|Acc], Segments).
split_segments([Code|Rest], CurrAcc, Acc, Segments) :-
    append(CurrAcc, [Code], NewAcc),
    split_segments(Rest, NewAcc, Acc, Segments).

validate_path_segment(Codes) :-
    maplist(valid_path_char, Codes).

% ZOS Path Parser
parse_zos_path([], _) :- fail.  % reject empty path
parse_zos_path(PathCodes, PathSegments) :-
    split_and_validate_zos_segments(PathCodes, PathSegments).

% Gestisce entrambi i casi: id44(id8) e solo id44
split_and_validate_zos_segments(Codes, [Id44, Id8]) :-
    append(Id44Codes, [40|Rest], Codes),  % split at (
    append(Id8Codes, [41|[]], Rest),      % deve terminare esattamente con )
    \+ member(40, Id44Codes),             % no ( in id44
    \+ member(41, Id44Codes),             % no ) in id44
    validate_id44_segment(Id44Codes, Id44),
    validate_id8_segment(Id8Codes, Id8),
    !.
split_and_validate_zos_segments(Codes, [Id44]) :-
    \+ member(40, Codes),                 % no parentesi aperte senza chiusura
    \+ member(41, Codes),                 % no parentesi chiuse senza apertura
    validate_id44_segment(Codes, Id44).

% Validazione più stringente per id44
validate_id44_segment(Codes, Id44) :-
    length(Codes, Len),
    Len > 0,
    Len =< 44,
    Codes = [First|_],
    char_type(First, alpha),              % deve iniziare con lettera
    \+ (append(_, [46], Codes)),          % non può terminare con punto
    \+ (append(_, [46,46|_], Codes)),     % non può avere punti consecutivi
    maplist(valid_id44_char, Codes),
    atom_codes(Id44, Codes).

% Validazione più stringente per id8
validate_id8_segment(Codes, Id8) :-
    length(Codes, Len),
    Len > 0,
    Len =< 8,
    Codes = [First|_],
    char_type(First, alpha),              % deve iniziare con lettera
    maplist(valid_id8_char, Codes),
    atom_codes(Id8, Codes).

% Caratteri validi per id44
valid_id44_char(Code) :-
    char_type(Code, alpha), !;            % lettere
    char_type(Code, digit), !;            % numeri
    Code = 46.                            % solo il punto è permesso

% Caratteri validi per id8 (più restrittivo, solo alfanumerici)
valid_id8_char(Code) :-
    char_type(Code, alpha), !;            % lettere
    char_type(Code, digit).               % numeri

% Decoding Functions
decode_segment(RawSegment, DecodedSegment) :-
    atom_codes(RawSegment, Codes),
    decode_percent_encoded(Codes, DecodedCodes),
    atom_codes(DecodedSegment, DecodedCodes).

valid_query_char(Code) :-
    Code \= 35,  % not #
    Code >= 32,  % printable characters
    Code =< 126.

valid_fragment_char(Code) :-
    Code >= 32,  % printable characters
    Code =< 126.

validate_query_chars(Chars) :-
    maplist(valid_query_char, Chars).

validate_fragment_chars(Chars) :-
    maplist(valid_fragment_char, Chars).

decode_query([], []).
decode_query(Codes, QueryPairs) :-
    validate_query_chars(Codes),
    split_query_params(Codes, ParamsList),
    maplist(decode_query_param, ParamsList, QueryPairs).

decode_fragment([], []).
decode_fragment(Codes, Fragment) :-
    validate_fragment_chars(Codes),
    decode_percent_encoded(Codes, DecodedCodes),
    atom_codes(Fragment, DecodedCodes).

split_query_params([], []).
split_query_params(Codes, [Param|Params]) :-
    append(ParamCodes, [38|Rest], Codes), % split at &
    !,
    Param = ParamCodes,
    split_query_params(Rest, Params).
split_query_params(Codes, [Codes]) :-
    Codes \= [].

decode_query_param(ParamCodes, [Name, Value]) :-
    (append(NameCodes, [61|ValueCodes], ParamCodes) -> % split at =
        decode_percent_encoded(NameCodes, DecodedNameCodes),
        atom_codes(Name, DecodedNameCodes),
        decode_percent_encoded(ValueCodes, DecodedValueCodes),
        atom_codes(Value, DecodedValueCodes)
    ;
        decode_percent_encoded(ParamCodes, DecodedNameCodes),
        atom_codes(Name, DecodedNameCodes),
        Value = ''
    ).

% Percent Encoding Decoder
decode_percent_encoded([], []).
decode_percent_encoded([37,H1,H2|Rest], [Char|Decoded]) :- % %
    hex_char(H1, D1),
    hex_char(H2, D2),
    Char is D1 * 16 + D2,
    decode_percent_encoded(Rest, Decoded).
decode_percent_encoded([Char|Rest], [Char|Decoded]) :-
    decode_percent_encoded(Rest, Decoded).

% Hex Character Decoder
hex_char(C, D) :-
    (C >= 48, C =< 57 ->    % 0-9
        D is C - 48
    ; C >= 65, C =< 70 ->   % A-F
        D is C - 55
    ; C >= 97, C =< 102 ->  % a-f
        D is C - 87
    ;
        throw(error(uri_parse_error('Invalid hex character'), _))
    ).
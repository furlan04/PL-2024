% Main URI Parser
urilib_parse(URIString, URI) :-
    atom_string(URIString, String),
    string_codes(String, Chars),
    parse_scheme(Chars, [], SchemeChars, Rest),
    atom_chars(Scheme, SchemeChars),
    scheme_parser(Scheme, Parser),
    catch(call(Parser, Rest, URI), _, fail).

% Scheme Parsers Registry
scheme_parser(http, parse_http_uri).
scheme_parser(https, parse_https_uri).
scheme_parser(ftp, parse_ftp_uri).
scheme_parser(mailto, parse_mailto_uri).
scheme_parser(news, parse_news_uri).
scheme_parser(tel, parse_tel_uri).
scheme_parser(fax, parse_fax_uri).
scheme_parser(zos, parse_zos_uri).

% Scheme Parser
parse_scheme([], _, _, _) :- fail.
parse_scheme([Char|Rest], Acc, Scheme, Remainder) :-
    (Char = 58 -> % colon found
        Scheme = Acc,
        Remainder = Rest
    ;
        append(Acc, [Char], NewAcc),
        parse_scheme(Rest, NewAcc, Scheme, Remainder)
    ).

% URI Components Parsers
parse_http_uri(Rest, uri(http, UserInfo, Host, Port, PathSegments, Query, Fragment)) :-
    parse_authority(Rest, http, Host, UserInfo, Port, Path),
    parse_path_query_fragment(Path, PathSegments, Query, Fragment),
    validate_port(Port, http).

parse_https_uri(Rest, uri(https, UserInfo, Host, Port, PathSegments, Query, Fragment)) :-
    parse_authority(Rest, https, Host, UserInfo, Port, Path),
    parse_path_query_fragment(Path, PathSegments, Query, Fragment),
    validate_port(Port, https).

parse_ftp_uri(Rest, uri(ftp, UserInfo, Host, Port, PathSegments, [], [])) :-
    parse_authority(Rest, ftp, Host, UserInfo, Port, Path),
    split_path_segments(Path, PathSegments),
    validate_port(Port, ftp).

parse_mailto_uri(Rest, uri(mailto, UserInfo, Host, 25, [], [], [])) :-
    parse_mailto_parts(Rest, UserInfo, Host).

parse_news_uri(Rest, uri(news, [], Host, 0, [], [], [])) :-
    atom_codes(Host, Rest),
    validate_host(Host).

parse_tel_uri(Rest, uri(tel, [UserInfo], '', 0, [], [], [])) :-
    parse_userinfo_tel(Rest, [], [UserInfo]).

parse_fax_uri(Rest, uri(fax, [UserInfo], '', 0, [], [], [])) :-
    parse_userinfo_tel(Rest, [], [UserInfo]).

parse_zos_uri(Rest, uri(zos, [], '', 0, PathSegments, [], [])) :-
    parse_zos_path(Rest, PathSegments).

% Authority Parser
parse_authority([47, 47|Authority], Scheme, Host, UserInfo, Port, Path) :-
    parse_authority_content(Authority, Scheme, Host, UserInfo, Port, Path).

parse_authority_content(Authority, Scheme, Host, UserInfo, Port, Path) :-
    (append(UserInfoChars, [64|HostPortPath], Authority) ->
        parse_userinfo(UserInfoChars, [], UserInfo),
        parse_host_port_path(HostPortPath, Scheme, Host, Port, Path)
    ;
        parse_host_port_path(Authority, Scheme, Host, Port, Path),
        UserInfo = []
    ).

% Host and Port Parser
parse_host_port_path(Input, Scheme, Host, Port, Path) :-
    (append(HostPort, [47|PathChars], Input) ->
        atom_codes(Path, [47|PathChars]),
        parse_host_port(HostPort, Scheme, Host, Port)
    ;
        parse_host_port(Input, Scheme, Host, Port),
        Path = ""
    ).

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
default_port(mailto, 25).
default_port(zos, 23).

% Host Validation
validate_host(Host) :-
    atom_codes(Host, Codes),
    (validate_ip_address(Codes) ; validate_domain_name(Codes)).

validate_ip_address(Codes) :-
    split_ip_segments(Codes, [], Segments),
    length(Segments, 4),
    maplist(validate_ip_segment, Segments).

validate_domain_name([First|Rest]) :-
    char_type(First, alpha),
    validate_domain_chars(Rest).

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
    atom_codes(UserInfo, Acc).
parse_userinfo([58|Rest], Acc, [Current|More]) :- % colon
    atom_codes(Current, Acc),
    parse_userinfo(Rest, [], More).
parse_userinfo([Char|Rest], Acc, UserInfo) :-
    append(Acc, [Char], NewAcc),
    parse_userinfo(Rest, NewAcc, UserInfo).

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
parse_path_query_fragment(Path, PathSegments, Query, Fragment) :-
    atom_codes(Path, PathCodes),
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
    (PathCodes = [47|Rest] -> % starts with /
        split_segments(Rest, [], [], ReversedSegments),
        reverse(ReversedSegments, RawSegments)
    ;
        split_segments(PathCodes, [], [], ReversedSegments),
        reverse(ReversedSegments, RawSegments)
    ),
    maplist(decode_segment, RawSegments, Segments).

split_segments([], [], Acc, Acc).
split_segments([], CurrAcc, Acc, [Segment|Acc]) :-
    atom_codes(Segment, CurrAcc).
split_segments([47|Rest], CurrAcc, Acc, Segments) :- % /
    atom_codes(Segment, CurrAcc),
    split_segments(Rest, [], [Segment|Acc], Segments).
split_segments([Code|Rest], CurrAcc, Acc, Segments) :-
    append(CurrAcc, [Code], NewAcc),
    split_segments(Rest, NewAcc, Acc, Segments).

% ZOS Path Parser
parse_zos_path([], []) :- !.
parse_zos_path(PathCodes, PathSegments) :-
    split_and_validate_zos_segments(PathCodes, PathSegments).

split_and_validate_zos_segments(Codes, [Id44, Id8]) :-
    append(Id44Codes, [40|Rest], Codes),  % split at (
    append(Id8Codes, [41], Rest),         % split at )
    validate_id44_segment(Id44Codes, Id44),
    validate_id8_segment(Id8Codes, Id8),
    !.
split_and_validate_zos_segments(Codes, [Id44]) :-
    validate_id44_segment(Codes, Id44).

validate_id44_segment(Codes, Id44) :-
    length(Codes, Len),
    Len > 0,
    Len =< 44,
    Codes = [First|_],
    char_type(First, alpha),
    maplist(valid_id44_char, Codes),
    atom_codes(Id44, Codes).

validate_id8_segment(Codes, Id8) :-
    length(Codes, Len),
    Len > 0,
    Len =< 8,
    Codes = [First|_],
    char_type(First, alpha),
    maplist(valid_id8_char, Codes),
    atom_codes(Id8, Codes).

valid_id44_char(Code) :-
    (char_type(Code, alnum) ; Code = 46).

valid_id8_char(Code) :-
    char_type(Code, alnum).

% Decoding Functions
decode_segment(RawSegment, DecodedSegment) :-
    atom_codes(RawSegment, Codes),
    decode_percent_encoded(Codes, DecodedCodes),
    atom_codes(DecodedSegment, DecodedCodes).

decode_query([], []).
decode_query(Codes, QueryPairs) :-
    split_query_params(Codes, ParamsList),
    maplist(decode_query_param, ParamsList, QueryPairs).

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

decode_fragment([], []).
decode_fragment(Codes, Fragment) :-
    decode_percent_encoded(Codes, DecodedCodes),
    atom_codes(Fragment, DecodedCodes).

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
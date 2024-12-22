% URI Parser
urilib_parse(URIString, URI) :-
    atom_string(URIString, String),
    string_codes(String, Chars),
    parse_scheme(Chars, [], SchemeChars, Rest),
    atom_chars(Scheme, SchemeChars),
    scheme_parser(Scheme, Parser),
    catch(call(Parser, Rest, URI), _, fail).

% Scheme parser mapping
scheme_parser(http, parse_http_uri).
scheme_parser(https, parse_https_uri).
scheme_parser(ftp, parse_ftp_uri).
scheme_parser(mailto, parse_mailto_uri).
scheme_parser(news, parse_news_uri).
scheme_parser(tel, parse_tel_uri).
scheme_parser(fax, parse_fax_uri).

% HTTP URI parser
parse_http_uri(Rest, uri(http, UserInfo, Host, Port, PathSegments, Query, Fragment)) :-
    parse_authority(Rest, http, Host, UserInfo, Port, FullPath),
    parse_path_query_fragment(FullPath, PathSegments, Query, Fragment),
    validate_port(Port, http).

% HTTPS URI parser
parse_https_uri(Rest, uri(https, UserInfo, Host, Port, PathSegments, Query, Fragment)) :-
    parse_authority(Rest, https, Host, UserInfo, Port, FullPath),
    parse_path_query_fragment(FullPath, PathSegments, Query, Fragment),
    validate_port(Port, https).

% FTP URI parser
parse_ftp_uri(Rest, uri(ftp, UserInfo, Host, Port, PathSegments, [], [])) :-
    parse_authority(Rest, ftp, Host, UserInfo, Port, FullPath),
    split_path_segments(FullPath, PathSegments),
    validate_port(Port, ftp).

% MAILTO URI parser
parse_mailto_uri(Rest, uri(mailto, UserInfo, Host, 25, [], [], [])) :-
    parse_mailto_parts(Rest, UserInfo, Host).

% News URI parser
parse_news_uri(Rest, uri(news, [], Host, 0, [], [], [])) :-
    % Per news non c'è // quindi prendiamo direttamente l'host
    atom_codes(Host, Rest),
    validate_host(Host).

% Scheme parsing
parse_scheme([], _, _, _) :- fail.
parse_scheme([Char | Rest], Acc, Scheme, Remainder) :-
    Char \= 58,  % Not a colon
    append(Acc, [Char], NewAcc),
    parse_scheme(Rest, NewAcc, Scheme, Remainder).
parse_scheme([58 | Rest], Acc, Acc, Rest).  % Found colon

% Authority parsing with scheme
parse_authority([47, 47 | Authority], Scheme, Host, UserInfo, Port, Path) :-
    parse_authority_content(Authority, Scheme, Host, UserInfo, Port, Path).

parse_authority_content(Authority, Scheme, Host, UserInfo, Port, Path) :-
    (  append(UserInfoChars, [64 | HostPortPath], Authority)
    -> parse_userinfo(UserInfoChars, [], UserInfo),
       parse_host_port_path(HostPortPath, Scheme, Host, Port, Path)
    ;  parse_host_port_path(Authority, Scheme, Host, Port, Path),
       UserInfo = []
    ).

% Host and port parsing with scheme-specific defaults
parse_host_port_path(Input, Scheme, Host, Port, Path) :-
    (  append(HostPort, [47 | PathChars], Input)
    -> atom_codes(Path, [47 | PathChars]),
       parse_host_port(HostPort, Scheme, Host, Port)
    ;  parse_host_port(Input, Scheme, Host, Port),
       Path = ""
    ).

parse_host_port(Input, Scheme, Host, Port) :-
    (  append(HostChars, [58], Input)  % Handle case where input ends with colon
    -> atom_codes(Host, HostChars),
       default_port(Scheme, Port),
       validate_host(Host)
    ;  append(HostChars, [58 | PortChars], Input)  % Normal case with port
    -> atom_codes(Host, HostChars),
       number_codes(Port, PortChars),
       validate_host(Host)
    ;  atom_codes(Host, Input),  % Case with no port specified
       default_port(Scheme, Port),
       validate_host(Host)
    ).

% Port validation with defaults
validate_port(0, Scheme) :- !,
    default_port(Scheme, Port),
    Port = Port.
validate_port(Port, _) :-
    integer(Port),
    Port > 0,
    Port < 65536.

% Default ports
default_port(http, 80).
default_port(https, 443).
default_port(ftp, 21).
default_port(mailto, 25).

% Host validation (either IP or domain)
validate_host(Host) :-
    atom_codes(Host, Codes),
    (  validate_ip_address(Codes)
    ;  validate_domain_name(Codes)
    ).

% IP address validation
validate_ip_address(Codes) :-
    split_ip_segments(Codes, [], Segments),
    length(Segments, 4),
    maplist(validate_ip_segment, Segments).

% Split IP address into segments
split_ip_segments([], CurrAcc, [LastSegment]) :-
    CurrAcc \= [],
    reverse_number_codes(CurrAcc, NumCodes),
    number_codes(LastSegment, NumCodes).
split_ip_segments([46|Rest], CurrAcc, [Segment|Segments]) :-  % 46 is '.'
    CurrAcc \= [],
    reverse_number_codes(CurrAcc, NumCodes),
    number_codes(Segment, NumCodes),
    split_ip_segments(Rest, [], Segments).
split_ip_segments([Digit|Rest], CurrAcc, Segments) :-
    char_type(Digit, digit),
    split_ip_segments(Rest, [Digit|CurrAcc], Segments).

% Validate IP segment (0-255)
validate_ip_segment(Segment) :-
    integer(Segment),
    Segment >= 0,
    Segment =< 255,
    !.

% Domain name validation
validate_domain_name([First|Rest]) :-
    char_type(First, alpha),
    validate_domain_chars(Rest).

validate_domain_chars([]).
validate_domain_chars([Char|Rest]) :-
    (char_type(Char, alnum)
    ;
    Char = 45  % hyphen
    ;
    Char = 46),  % dot
    validate_domain_chars(Rest).

% User info parsing
parse_userinfo([], Acc, [UserInfo]) :-
    atom_codes(UserInfo, Acc).
parse_userinfo([58 | Rest], Acc, [Current | More]) :-  % 58 is ':'
    atom_codes(Current, Acc),
    parse_userinfo(Rest, [], More).
parse_userinfo([Char | Rest], Acc, UserInfo) :-
    append(Acc, [Char], NewAcc),
    parse_userinfo(Rest, NewAcc, UserInfo).

% Mailto parsing
parse_mailto_parts(Chars, UserInfo, Host) :-
    (append(BeforeAt, [64|HostPart], Chars) ->
        atom_codes(HostAtom, HostPart),
        validate_host(HostAtom),
        Host = HostAtom,
        parse_userinfo(BeforeAt, [], UserInfo)
    ;   
        parse_userinfo(Chars, [], UserInfo),
        Host = '').

% Tel URI parser
parse_tel_uri(Rest, uri(tel, [UserInfo], '', 0, [], [], [])) :-
    % Per tel prendiamo tutto come userinfo
    parse_userinfo_tel(Rest, [], [UserInfo]).

parse_fax_uri(Rest, uri(fax, [UserInfo], '', 0, [], [], [])) :-
    % Per tel prendiamo tutto come userinfo
    parse_userinfo_tel(Rest, [], [UserInfo]).

% Parser specifico per tel userinfo che non richiede validazione speciale
parse_userinfo_tel([], Acc, [UserInfo]) :-
    atom_codes(UserInfo, Acc).
parse_userinfo_tel([Char | Rest], Acc, UserInfo) :-
    append(Acc, [Char], NewAcc),
    parse_userinfo_tel(Rest, NewAcc, UserInfo).

% Path, query and fragment parsing with percent-decoding
parse_path_query_fragment(Path, PathSegments, Query, Fragment) :-
    atom_codes(Path, PathCodes),
    split_path_query_fragment(PathCodes, PathCodes1, QueryCodes, FragmentCodes),
    decode_path_segments(PathCodes1, PathSegments),
    decode_query(QueryCodes, Query),
    decode_fragment(FragmentCodes, Fragment).

% Split path, query, and fragment
split_path_query_fragment(Codes, PathCodes, QueryCodes, FragmentCodes) :-
    split_at_char(Codes, 35, BeforeHash, FragmentCodes),  % 35 is '#'
    split_at_char(BeforeHash, 63, PathCodes, QueryCodes). % 63 is '?'

% Helper to split at character
split_at_char(Codes, Char, Before, After) :-
    (  append(Before, [Char|After], Codes)
    -> true
    ;  Before = Codes,
       After = []
    ).

% Path segment handling
decode_path_segments(PathCodes, Segments) :-
    (PathCodes = [47 | Rest] ->
        split_segments_helper(Rest, [], [], ReversedSegments),
        reverse(ReversedSegments, RawSegments)
    ;
        split_segments_helper(PathCodes, [], [], ReversedSegments),
        reverse(ReversedSegments, RawSegments)
    ),
    maplist(decode_segment, RawSegments, Segments).

% Split path segments
split_path_segments("", []).  % Handle empty path case
split_path_segments(Path, Segments) :-
    atom_codes(Path, PathCodes),
    (PathCodes = [47 | Rest] ->  % If starts with '/'
        split_segments_helper(Rest, [], [], ReversedSegments),
        reverse(ReversedSegments, Segments)
    ;
        split_segments_helper(PathCodes, [], [], ReversedSegments),
        reverse(ReversedSegments, Segments)
    ).

split_segments_helper([], [], Acc, Acc) :- !.
split_segments_helper([], CurrAcc, Acc, [Segment|Acc]) :-
    atom_codes(Segment, CurrAcc).
split_segments_helper([47|Rest], CurrAcc, Acc, Segments) :-  % 47 is '/'
    atom_codes(Segment, CurrAcc),
    split_segments_helper(Rest, [], [Segment|Acc], Segments).
split_segments_helper([Code|Rest], CurrAcc, Acc, Segments) :-
    append(CurrAcc, [Code], NewAcc),
    split_segments_helper(Rest, NewAcc, Acc, Segments).

% Decode individual path segment
decode_segment(RawSegment, DecodedSegment) :-
    atom_codes(RawSegment, Codes),
    decode_percent_encoded(Codes, DecodedCodes),
    atom_codes(DecodedSegment, DecodedCodes).

% Query string parsing into simple key-value pairs
decode_query([], []).
decode_query(Codes, QueryPairs) :-
    split_query_params(Codes, ParamsList),
    maplist(decode_query_param, ParamsList, QueryPairs).

split_query_params(Codes, [Param|Params]) :-
    append(ParamCodes, [38|Rest], Codes),  % 38 is '&'
    !,
    Param = ParamCodes,
    split_query_params(Rest, Params).
split_query_params(Codes, [Codes]) :-
    Codes \= [].

decode_query_param(ParamCodes, [Name, Value]) :-
    (  append(NameCodes, [61|ValueCodes], ParamCodes)  % 61 is '='
    -> decode_percent_encoded(NameCodes, DecodedNameCodes),
       atom_codes(Name, DecodedNameCodes),
       decode_percent_encoded(ValueCodes, DecodedValueCodes),
       atom_codes(Value, DecodedValueCodes)
    ;  decode_percent_encoded(ParamCodes, DecodedNameCodes),
       atom_codes(Name, DecodedNameCodes),
       Value = ''
    ).

% Fragment decoding
decode_fragment([], []).
decode_fragment(Codes, Fragment) :-
    decode_percent_encoded(Codes, DecodedCodes),
    atom_codes(Fragment, DecodedCodes).

% Percent-encoding decoder
decode_percent_encoded([], []).
decode_percent_encoded([37,H1,H2|Rest], [Char|Decoded]) :- % 37 is '%'
    hex_char(H1, D1),
    hex_char(H2, D2),
    Char is D1 * 16 + D2,
    decode_percent_encoded(Rest, Decoded).
decode_percent_encoded([Char|Rest], [Char|Decoded]) :-
    decode_percent_encoded(Rest, Decoded).

% Hex character to decimal
hex_char(C, D) :-
    (  C >= 48, C =< 57  % 0-9
    -> D is C - 48
    ;  C >= 65, C =< 70  % A-F
    -> D is C - 55
    ;  C >= 97, C =< 102 % a-f
    -> D is C - 87
    ;  throw(error(uri_parse_error('Invalid hex character in percent-encoding'), _))
    ).

% Helper predicate to reverse number codes
reverse_number_codes(Codes, NumCodes) :-
    reverse(Codes, NumCodes).
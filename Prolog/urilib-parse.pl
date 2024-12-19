% Main predicate to parse URI string by selecting appropriate parser based on scheme
urilib_parse(URIString, URI) :-
    atom_string(URIString, String),
    string_codes(String, Chars),
    parse_scheme(Chars, [], SchemeChars, Rest),
    atom_chars(Scheme, SchemeChars),
    parse_by_scheme(Scheme, Rest, URI).

% Specific handler for HTTP scheme
parse_by_scheme(http, Rest, uri(http, UserInfo, Host, Port, PathSegments, Query, Fragment)) :- !,
    parse_http_authority(Rest, Host, UserInfo, Port, [], FullPath),
    parse_http_path_query_fragment(FullPath, PathSegments, Query, Fragment).

% Fail for non-HTTP schemes (to be implemented separately)
parse_by_scheme(Scheme, _, _) :-
    Scheme \= http,
    false.

% Parse scheme up to the colon
parse_scheme([], _, _, _) :- fail.
parse_scheme([Char | Rest], Acc, Scheme, Remainder) :-
    Char \= 58,  % Not a colon
    append(Acc, [Char], NewAcc),
    parse_scheme(Rest, NewAcc, Scheme, Remainder).
parse_scheme([58 | Rest], Acc, Acc, Rest).

% HTTP-specific authority parsing
parse_http_authority([47, 47 | Authority], Host, UserInfo, Port, _, Path) :-
    parse_http_authority_content(Authority, Host, UserInfo, Port, Path).

% HTTP-specific authority content parsing - with UserInfo
parse_http_authority_content(Authority, Host, UserInfo, Port, Path) :-
    append(UserInfoChars, [64 | HostPortPath], Authority), !,
    parse_userinfo(UserInfoChars, [], UserInfo),
    parse_http_host_port_path(HostPortPath, Host, Port, Path).

% HTTP-specific authority content parsing - without UserInfo
parse_http_authority_content(Authority, Host, [], Port, Path) :-
    parse_http_host_port_path(Authority, Host, Port, Path).

% HTTP-specific host, port, and path parsing
parse_http_host_port_path(Input, Host, Port, Path) :-
    (append(HostPort, [47 | PathChars], Input) ->
        atom_codes(Path, [47 | PathChars]),
        parse_http_host_port(HostPort, Host, Port)
    ;
        parse_http_host_port(Input, Host, Port),
        Path = "").

% HTTP-specific host and port parsing
parse_http_host_port(Input, Host, Port) :-
    (append(HostChars, [58 | PortChars], Input) ->
        atom_codes(Host, HostChars),
        number_codes(Port, PortChars)
    ;
        atom_codes(Host, Input),
        Port = 80).  % Default HTTP port

% Rest of the predicates remain the same but renamed to indicate HTTP specificity
parse_http_path_query_fragment(Path, PathSegments, Query, Fragment) :-
    atom_codes(Path, PathCodes),
    % First find fragment
    (append(BeforeHash, [35|FragmentCodes], PathCodes) ->
        atom_codes(Fragment, FragmentCodes),
        (append(BeforeQuery, [63|QueryCodes], BeforeHash) ->
            atom_codes(QueryAtom, QueryCodes),
            Query = [QueryAtom],
            PathToProcess = BeforeQuery
        ;   
            Query = [],
            PathToProcess = BeforeHash)
    ;   
        Fragment = [],
        (append(BeforeQuery, [63|QueryCodes], PathCodes) ->
            atom_codes(QueryAtom, QueryCodes),
            Query = [QueryAtom],
            PathToProcess = BeforeQuery
        ;   
            Query = [],
            PathToProcess = PathCodes)
    ),
    split_path_segments(PathToProcess, PathSegments).

% Helper predicates remain the same
parse_userinfo([], Acc, [UserInfo]) :-
    atom_codes(UserInfo, Acc).
parse_userinfo([58 | Rest], Acc, [Current | More]) :-
    atom_codes(Current, Acc),
    parse_userinfo(Rest, [], More).
parse_userinfo([Char | Rest], Acc, UserInfo) :-
    append(Acc, [Char], NewAcc),
    parse_userinfo(Rest, NewAcc, UserInfo).

split_path_segments(PathCodes, Segments) :-
    (PathCodes = [47 | Rest] ->
        split_segments_helper(Rest, [], [], ReversedSegments),
        reverse(ReversedSegments, Segments)
    ;
        split_segments_helper(PathCodes, [], [], ReversedSegments),
        reverse(ReversedSegments, Segments)).

split_segments_helper([], [], Acc, Acc) :- !.
split_segments_helper([], CurrAcc, Acc, [Segment|Acc]) :-
    atom_codes(Segment, CurrAcc).
split_segments_helper([47|Rest], CurrAcc, Acc, Segments) :-
    atom_codes(Segment, CurrAcc),
    split_segments_helper(Rest, [], [Segment|Acc], Segments).
split_segments_helper([Code|Rest], CurrAcc, Acc, Segments) :-
    append(CurrAcc, [Code], NewAcc),
    split_segments_helper(Rest, NewAcc, Acc, Segments).
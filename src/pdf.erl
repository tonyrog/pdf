%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    PDF 
%%% @end
%%% Created : 11 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(pdf).

-export([save/2]).
-export([load/1]).
-export([info/1]).
-export([new/0]).
-export([add_object/1]).
-export([set_root/2, get_root/1]).
-export([add_catalog/2]).
-export([add_pages/2]).
-export([add_page/2]).
-export([add_contents/3]).

-export([format_int/1,format_float/1,format_number/1,format_boolean/1]).
-export([format_name/1, format_string/1, format_hex_string/1]).
-export([format_array/1, format_dict/1]).
-export([format_obj/1]).
-export([font_list/0]).

-export([parse_int/1,parse_unsigned/1,parse_float/1]).
-export([parse_array/1]).

-export([filter_ASCIIHexEncode/1,filter_ASCIIHexDecode/2]).
-export([filter_ASCII85Encode/1,filter_ASCII85Decode/2]).
-export([filter_FlateEncode/1, filter_FlateDecode/2]).
-export([filter_LZWEncode/1, filter_LZWDecode/2]).
-export([decode_string/1]).
-compile(export_all).

-define(BL, $\s).
-define(NL, $\n).
-define(is_range(C,A,B), (((C)>=(A)) andalso ((C)=<(B)))).
-define(is_upper(C), ?is_range((C),$A,$Z)).
-define(is_lower(C), ?is_range((C),$a,$z)).
-define(is_digit(C), ?is_range((C),$0,$9)).
-define(is_octal(C), ?is_range((C),$0,$7)).
-define(is_xdigit(C),
	(?is_range((C),$0,$9) orelse 
	 ?is_range((C),$A,$F) orelse ?is_range((C),$a,$f))).
-define(is_keyword1(C), (?is_lower((C)) orelse ?is_upper((C)) 
			 orelse ((C) =:= $'))).
-define(is_keyword2(C), (?is_lower((C)) orelse ?is_upper((C))
			 orelse ?is_digit((C)) 
			 orelse ((C) =:= $')
			 orelse ((C) =:= $*))).
-define(is_objref(R), (is_list(R) andalso (hd(R) =:= 'R'))).

%%-define(xdbg(F,A), io:format((F),(A))).
-define(xdbg(F,A), ok).
%%-define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).
-define(dbg0(F,A), ok).
-define(dbg1(F,A), io:format((F),(A))).

%% Erlang is dynamically typed and lists may contain items of mixed types!!!
%% and so do many other languages (read Python!)
-type hex_string() :: binary().
-type literal_string() :: string().
-type pdf_string() :: hex_string() | literal_string().
-type pdf_array() :: tuple().
-type pdf_dictionary() :: #{ atom() => pdf_object() }.
%% ['stream',Dict,Data::binary() | [{Cmd,Args}]
-type pdf_stream() ::
	nonempty_improper_list(
	  stream,
	  nonempty_improper_list(
	    Dict::pdf_dictionary(),
	    nonempty_improper_list(Data::term(),[]))).
%% [obj,N::integer,G::integer(),Obj::term()] !? suger? silly stuff
-type pdf_indirect_object() ::
	nonempty_improper_list(
	  obj,
	  nonempty_improper_list(
	    N::integer(),
	    nonempty_improper_list(
	      G::integer(),
	      nonempty_improper_list(Obj::pdf_object() |
					  pdf_stream(), [])))).
%% ['R',N::integer,G::integer()]
-type pdf_objref() ::
	nonempty_improper_list(
	  'R',
	  nonempty_improper_list(
	    N::integer(), nonempty_improper_list(G::integer(), []))).

%% ['F',N::integer,G::integer()]
-type pdf_freeref() ::
	nonempty_improper_list(
	  'F',
	  nonempty_improper_list(
	    N::integer(), nonempty_improper_list(G::integer(), []))).

-type pdf_object() :: 
	boolean() | integer() | float() | 
	pdf_string() | atom() | pdf_array() |
	pdf_dictionary() |
	pdf_objref().

-type pdf_xref() ::
	#{ pdf_objref() => Offset::integer(),
	   pdf_objref() => {ParentNum::integer(),Index::integer()},
	   pdf_freeref() => Next::integer() }.

-type pdf_trailer() ::
	#{ 'Size' => integer(),
	   'Root' => pdf_dictionary(),
	   'Prev' => integer(),
	   'Encrypt' => pdf_dictionary(),
	   'Info' =>  pdf_dictionary(),
	   'ID' => pdf_array()
	 }.

-type pdf_number_tree() ::
	#{ 'Kids' => pdf_array(),
	   'Nums' => pdf_array(),
	   'Limits' => pdf_array() }.
	
-type pdf() :: #{ pdf_objref() => pdf_indirect_object(),
		  '$XRef' => pdf_xref(),
		  '$XRefOffset' => integer(),
		  '$Trailer' => pdf_trailer()
		}.

-type pdf_tree() :: pdf_objref() |  %% leaf
		    { pdf_objref(), [pdf_tree()]}.  %% node

-include("pdf.hrl").

-spec new() -> pdf().

new() ->
    Trailer = #{ 'Size' => 1 },
    #{ '$Trailer' => Trailer }.

-spec get_trailer(PDF::pdf()) -> pdf_trailer().
get_trailer(PDF) ->
    maps:get('$Trailer', PDF, #{}).

set_trailer(PDF, Trailer) ->
    maps:put('$Trailer', Trailer, PDF).

-spec add_object(PDF::pdf()) -> {Ref::pdf_objref(), NewPDF::pdf()}.
add_object(PDF) ->
    add_object(PDF, ?null).

-spec add_object(PDF::pdf(),Object::pdf_object()) ->
	  {Ref::pdf_objref(), NewPDF::pdf()}.
add_object(PDF, Object) ->
    Trailer = get_trailer(PDF),
    N = get_named_value('Size', Trailer, 1),
    Ref = ?objref(N,0),
    PDF1 = PDF#{  Ref => ?object(N,0,Object) },
    PDF2 = set_trailer(PDF1, Trailer#{ 'Size' => N+1 }),
    {Ref, PDF2}.

%% set document tree root
set_root(PDF, Ref) ->
    Trailer = get_trailer(PDF),
    set_trailer(PDF, Trailer#{ 'Root' => Ref }).

-spec get_root(PDF::pdf()) -> undefined | pdf_objref().
get_root(PDF) ->
    Trailer = get_trailer(PDF),
    get_named_value('Root',Trailer,undefined).

set_parent(PDF, Ref, ParentRef) ->
    set_field(PDF, Ref, 'Parent', ParentRef).

set_field(PDF, Ref, Field, Value) when ?is_objref(Ref), is_atom(Field) ->
    %% fixme: (debug) validate that Field exist in object type
    ?object(N,G,Dict) = maps:get(Ref, PDF),
    Dict1 = Dict#{ Field => Value },
    PDF#{ Ref => ?object(N,G,Dict1) }.

%% add an empty catalog node
add_catalog(PDF,PagesRef) ->
    Object = #{ 'Type' => 'Catalog',
		'Pages' => PagesRef },
    add_object(PDF, Object).

get_catalog(PDF) ->
    Root = get_root(PDF),
    RootObj = get_object(Root, PDF),
    'Catalog' = get_named_value('Type', RootObj), %% check catalog type
    RootObj.

%% fetch PageLabels - number tree
-spec get_page_labels(PDF::pdf()) -> pdf_number_tree()|pdf_objref().
get_page_labels(PDF) ->
    Catalog = get_catalog(PDF),
    get_named_value('PageLabels', Catalog, #{}).

%% add empty pages node
add_pages(PDF,Parent) ->
    Object = #{ 'Type' => 'Pages',
		'Parent' => Parent,
		'Kids' => {},
		'Count' => 0 },
    add_object(PDF, Object).

%% add a new page to pages ref
add_page(PDF,PagesRef) ->
    ?object(N,G,PagesDict) = maps:get(PagesRef, PDF, #{}),
    Kids  = maps:get('Kids', PagesDict, {}),
    Object = #{ 'Type' => 'Page', 'Parent' => PagesRef },
    {PageRef,PDF1} = add_object(PDF, Object),
    Kids1 = append_array(Kids, PageRef),
    PagesDict1 = maps:put('Kids',Kids1,PagesDict),
    {PageRef,maps:put(PagesRef, ?object(N,G,PagesDict1), PDF1)}.

add_contents(PDF, PageRef, StreamOrArray) ->
    ?object(N,G,PageDict) = maps:get(PageRef, PDF, #{}),
    Contents0 = maps:get('Contents', PageDict, {}),
    Contents = append_array(Contents0, StreamOrArray),
    PageDict1 = PageDict#{ 'Contents' => Contents },
    maps:put(PageRef, ?object(N,G,PageDict1), PDF).

-spec save(Filename::string(), PDF::pdf()) ->
	  ok | {error,Reason::term()}.

%% fixme: add encode/compress/version options etc
save(Filename, PDF) ->
    case file:open(Filename, [write]) of
	{ok,Fd} ->
	    write_header(Fd, ?PDF_VERSION),
	    XRef = write_obj_list(Fd, PDF, []),
	    XRefOffset = write_xref(Fd,XRef),
	    [obj,RootN,RootG|_] = hd(PDF),
	    Trailer = #{ 'Size' => length(PDF),
			 'Root' => ?objref(RootN,RootG) },
	    write_trailer(Fd, Trailer, XRefOffset),
	    file:close(Fd);
	Error ->
	    Error
    end.

write_header(Fd, {Major,Minor}) ->
    ok = file:write(Fd, ["%PDF-", integer_to_list(Major),".",
			 integer_to_list(Minor),"\n"]).

write_trailer(Fd, Trailer, XRefOffset) ->
    file:write(Fd, "trailer\n"),
    write_obj(Fd, Trailer),
    file:write(Fd, "\nstartxref\n"),
    file:write(Fd, [integer_to_list(XRefOffset),"\n"]),
    file:write(Fd, "%%EOF\n").
     
write_obj_list(Fd, [Obj=?object(N,G,_)|ObjList], XRef) ->
    Offset = write_obj(Fd, Obj),
    write_obj_list(Fd, ObjList, [{Offset,N,G}|XRef]);
write_obj_list(_Fd, [], XRef) ->
    lists:keysort(2,XRef).

write_obj(Fd, Obj) ->
    {ok,Offset} = file:position(Fd, cur),
    file:write(Fd, format_obj(Obj)),
    Offset.

write_xref(Fd, XRef) ->
    {ok,Offset} = file:position(Fd, cur),
    file:write(Fd, "xref\n"),
    write_xref_sections(Fd, XRef),
    Offset.

write_xref_sections(_Fd, []) ->
    ok;
write_xref_sections(Fd, XRef) ->
    {Section,XRef1} = get_section(XRef),
    [{_,N,_G}|_] = Section,
    file:write(Fd, [integer_to_list(N)," ",
		    integer_to_list(length(Section)),"\n"]),
    %% io:format(Fd, "~w ~w\n", [N, length(Section)]),
    lists:foreach(
      fun({Offset,_N,G}) ->
	      Entry = io_lib:format("~10..0w ~5..0w n \n", [Offset,G]),
	      ok = file:write(Fd, Entry)
      end, Section),
    write_xref_sections(Fd, XRef1).

get_section([E={_,N,_}|XRef]) ->
    get_section(XRef, N, [E]).
get_section([E={_,M,_}|XRef], N, Es) when N+1 =:= M ->
    get_section(XRef, M, [E|Es]);
get_section(XRef, _N, Es) ->
    {lists:reverse(Es), XRef}.

-spec load(Filename::string()) -> pdf() | {error,Reason::term()}.

load(Filename) ->
    case file:open(Filename, [read]) of
	{ok,Fd} ->
	    try load_(Fd, Filename) of
		PDF -> PDF
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

load_(Fd, Filename) ->
    %% check magic and read version
    Cs = get_char_fd_fun(Fd),
    {Magic,Cs1} = get_data(Cs,10),
    case Magic of
	<<$%,$P,$D,$F,$-,A,$.,B,M,N>> ->
	    Vsn = {A-$0,B-$0},
	    EOL = if M =:= $\n -> nl;
		     M =:= $\r, M =/= $\n -> cr;
		     M =:= $\s, N =:= $\n -> nl;
		     M =:= $\r, N =:= $\n -> crnl
		  end,
	    {ok,Offset0} = file:position(Fd, cur),
	    {BData,_Cs2} = get_data(Cs1,5),
	    BOffs = if M =:= $\n, N =:= $% -> 0;
		       M =:= $\r, N =:= $\n -> 1;
		       true -> 0
		    end,
	    Binary = case BData of
			 <<_:BOffs/binary,C1,C2,C3,C4,_/binary>> ->
			     ((C1 band C2 band C3 band C4) band 16#80)=:=16#80
		     end,
	    Info = #{ version => Vsn,
		      filename => Filename,
		      eol => EOL,
		      binary => Binary },
	    case load_xref_offset(Fd) of
		false ->
		    {ok,_} = file:position(Fd, Offset0),
		    load_seq_objects(Fd, #{ info => Info });
		{ok,Offset} ->
		    PDF0 = #{ info => Info,
			      '$XRefOffset' => Offset, %% main offset
			      '$XRef' => #{} },
		    Trailer0 = #{},
		    {Trailer1,PDF1} = load_xref(Fd,Offset,Trailer0,PDF0),
		    PDF2 = set_trailer(PDF1, Trailer1),
		    load_objects(Fd, PDF2)
	    end;
	_->
	    {error, bad_magic}
    end.

load_xref_offset(Fd) ->
    file:position(Fd, {eof,-64}),
    Read = get_char_fd_fun(Fd),
    {Data,_} = get_data(Read, 64), %% assume Data is binary
    find_xref_offset(Data).

find_xref_offset(<<"startxref",Cs/binary>>) ->
    case get_char(Cs) of
	[C|Cs1] ->
	    case is_white_space(C) of
		true ->
		    Cs2 = skip_white_space(Cs1),
		    {Offset,_Cs3} = parse_unsigned(Cs2),
		    {ok,Offset};
		false ->
		    false
	    end;
	eof ->
	    false
    end;
find_xref_offset(<<_,Cs1/binary>>) ->
    find_xref_offset(Cs1);
find_xref_offset(<<>>) ->
    false.


%% Load when xref table is not found
load_seq_objects(Fd, PDF) ->
    Read = get_char_fd_fun(Fd),
    load_seq_objects_(Fd, Read, PDF).

load_seq_objects_(Fd, Cs, PDF) ->
    case parse_obj(Cs) of
	{N,Cs1} when is_integer(N) ->
	    {Obj,Cs2,PDF1} = parse_indirect_object2(Fd,Cs1,N,PDF),
	    ?dbg0("Loaded object ~p\n", [Obj]),
	    [obj,N,_G,_] = Obj,
	    load_seq_objects_(Fd, Cs2, PDF1);
	{?key(trailer), Cs1} ->
	    {Trailer,_Cs2} = parse_dict(Cs1),
	    set_trailer(PDF, Trailer)
    end.
    
%% load all object in xref table - fixme, only load when needed?
load_objects(Fd, PDF) ->
    load_objects(Fd, maps:to_list(maps:get('$XRef',PDF)), PDF).
    
load_objects(Fd, [{Ref=?objref(_N,_G),Offset}|XRef], PDF) 
  when is_integer(Offset) ->
    ?dbg("load object ~p = ~w\n", [Ref,Offset]),
    {_Obj,PDF1} = load_object(Fd, Ref, PDF),
    load_objects(Fd, XRef, PDF1);
load_objects(Fd, [{Ref=?objref(_N,_G),{_Pn,_Pi}}|XRef], PDF) ->
    ?dbg("load object stream ~p = ~w\n", [Ref,{_Pn,_Pi}]),
    {_Obj,PDF1} = load_object(Fd, Ref, PDF),
    load_objects(Fd, XRef, PDF1);
load_objects(Fd, [{?freeref(_N,_G),_Offset}|XRef], PDF) ->
    load_objects(Fd, XRef, PDF);
load_objects(_Fd, [], PDF) ->
    PDF.

%% lookup or load object: fixme detect recursive load!
load_object(Fd,Ref=?objref(N,G),PDF) ->
    ?dbg("load object ~w ~w\n", [N,G]),
    case maps:get(Ref, PDF, undefined) of
	Obj = ?object(N,G,_Data) ->
	    ?dbg("  already loaded\n", []),
	    {Obj,PDF};
	undefined ->  %% is not loaded?
	    XRef = maps:get('$XRef', PDF, #{}),
	    case maps:get(Ref, XRef, undefined) of
		undefined ->
		    ?dbg1("object ~w:~w not found\n", [N,G]),
		    {?null,PDF}; %% object is missing
		Offset when is_integer(Offset) ->
		    {ok,Save} = file:position(Fd, cur),
		    ?dbg("save offset = ~w\n", [Save]),
		    file:position(Fd, Offset),
		    ?dbg("set offset = ~w\n", [Offset]),
		    Read = get_char_fd_fun(Fd),
		    {Obj,_Cs,PDF1} = parse_indirect_object(Fd,Read,PDF),
		    ?dbg("  loaded ~p\n", [Obj]),
		    file:position(Fd, Save), %% restore
		    ?dbg("restore offset = ~w\n", [Save]),
		    {Obj,PDF1};
		{Pn,Pi} ->
		    %% load object i'th (Pi) from object stream Pn:0
		    {PObj,PDF1} = load_object(Fd,?objref(Pn,0),PDF),
		    [obj,Pn,0,[stream,Dict,Data]] = PObj,
		    'ObjStm' = maps:get('Type',Dict), %% check type
		    NObjs = maps:get('N',Dict),
		    First = maps:get('First',Dict),
		    %% Extends?
		    %% Now parse the Parse ( ObjNum ObjOffset){N}
		    Header = parse_objstm_header(NObjs, Data),
		    ?dbg("ObjStm Header = ~w\n", [Header]),
		    {_Num,Offs} = element(Pi+1, Header),
		    Offset = First + Offs,
		    ?dbg("parse Obj ~w@~w\n", [Num, Offset]),
		    <<_:Offset/binary,Data1/binary>> = Data,
		    {Obj,_Cs1} = parse_obj(Data1),
		    ?dbg("Obj ~w@~w =~w\n", [Num, Offset,Obj]),
		    Object = ?object(N,G,Obj),
		    PDF2 = PDF1# { Ref => Object },
		    {Object,PDF2}
	    end
    end.

parse_objstm_header(N, Cs) ->
    parse_objstm_header_(N, Cs, []).
parse_objstm_header_(0, _Cs, Acc) ->
    list_to_tuple(lists:reverse(Acc));
parse_objstm_header_(N, Cs, Acc) ->
    Cs1 = skip_white_space(Cs),
    {Num,Cs2} = parse_unsigned(Cs1),
    Cs3 = skip_white_space(Cs2),
    {Offs,Cs4} = parse_unsigned(Cs3),
    parse_objstm_header_(N-1, Cs4, [{Num,Offs}|Acc]).


load_xref(Fd,Offset,Trailer0,PDF) ->
    file:position(Fd, Offset),
    ?xdbg("Load xref from offset=~w\n", [Offset]),
    {Trailer1,PDF1} = load_xref(Fd,Trailer0,PDF),
    Prev = maps:get('Prev',Trailer1,undefined),
    Trailer2 = maps:merge(Trailer0,maps:remove('Prev',Trailer1)),
    if Prev =:= undefined ->
	    {Trailer2,PDF1};
       true ->
	    file:position(Fd, Prev),
	    ?xdbg("Load xref from offset=~w\n", [Prev]),
	    load_xref(Fd,Trailer2,PDF1)
    end.

load_xref(Fd,Trailer0,PDF) ->
    Cs = get_char_fd_fun(Fd),
    case parse_obj(Cs) of
	{N,Cs1} when is_integer(N) ->
	    load_xref_stream(Fd,Cs1,N,Trailer0,PDF);
	{?key(xref), Cs1} ->
	    Cs2 = skip_newline(Cs1),
	    ?xdbg("Load XRef entires from XRef table\n",[]),
	    {XRef,Trailer1} = load_xref_table(Fd,Cs2,maps:get('$XRef',PDF)),
	    ?xdbg("XRef trailer = ~p\n", [Trailer1]),
	    PDF1 = PDF#{ '$XRef' => XRef },
	    Trailer2 = maps:merge(Trailer0,Trailer1),
	    {Trailer2,PDF1}
    end.

%% load (N) G obj .. 
load_xref_stream(Fd,Cs,N,Trailer0,PDF) ->
    {Obj,_Cs1,PDF1} = parse_indirect_object2(Fd,Cs,N,PDF),
    ?object(N,_G,?stream(Trailer1,Data)) = Obj,
    'XRef' = maps:get('Type',Trailer1),
    Prev = maps:get('Prev',Trailer1,undefined),
    W = maps:get('W',Trailer1),
    Size = maps:get('Size',Trailer1),
    Index = maps:get('Index',Trailer1,{0,Size}),
    ?xdbg("Load Xref stream ~w:~w\n", [N,_G]),
    ?xdbg("Trailer1=~w, W=~w, Size=~w, Index=~w\n",[Trailer1,W,Size,Index]),
    XRef = load_xref_entries(W,1,Index,Data,maps:get('$XRef',PDF1)),
    PDF2 = PDF1#{ '$XRef' => XRef },
    Trailer2 = maps:merge(Trailer0,maps:remove('Prev',Trailer1)),
    if Prev =:= undefined ->
	    {Trailer2, PDF2};
       true ->
	    file:position(Fd, Prev),
	    load_xref(Fd,Trailer2,PDF2)
    end.

load_xref_entries(TNG,K,Index,Data,XRef) ->
    if K < tuple_size(Index) ->
	    load_xref_entries(TNG,K,
			      element(K,Index),
			      element(K+1,Index),
			      Index,Data,XRef);
       true ->
	    XRef
    end.

load_xref_entries(TNG={T,N,G},K,_Num,0,Index,Data,XRef) ->
    load_xref_entries(TNG={T,N,G},K+2,Index,Data,XRef);
load_xref_entries(TNG={T,N,G},K,Num,I,Index,Data,XRef) ->
    case Data of
	<<0:T/unit:8, Next:N/unit:8, Gen:G/unit:8, Data1/binary>> ->
	    load_xref_entries(TNG,K,Num+1,I-1,Index,Data1,
			      add_xref_entry(?freeref(Num,Gen),Next,XRef));
	<<1:T/unit:8, Offset:N/unit:8, Gen:G/unit:8, Data1/binary>> ->
	    load_xref_entries(TNG,K,Num+1,I-1,Index,Data1,
			      add_xref_entry(?objref(Num,Gen),Offset,XRef));
	<<2:T/unit:8, Pn:N/unit:8, Pi:G/unit:8, Data1/binary>> ->
	    load_xref_entries(TNG,K,Num+1,I-1,Index,Data1,
			      add_xref_entry(?objref(Num,0),{Pn,Pi},XRef))
    end.

%%
%% load
%%    N1 M1
%%    nnnnnnnnnn ggggg n eol
%%    nnnnnnnnnn ggggg f eol
%%    ...
%%    N2 M2
%%    nnnnnnnnnn ggggg n eol
%%    nnnnnnnnnn ggggg f eol
%%    ...
%%    trailer <<dict>>
%%

load_xref_table(Fd,Cs,XRef) ->
    case parse_obj(Cs) of
	{?key(trailer), Cs1} ->
	    {Trailer1,_Cs2} = parse_dict(Cs1),
	    {XRef,Trailer1};
	{First,Cs1} ->
	    {Num,Cs2} = parse_obj(Cs1),
	    Cs3 = skip_white_space(Cs2),
	    Cs4 = skip_newline(Cs3),
	    load_xref_table(Fd,Cs4,First,Num,XRef)
    end.

load_xref_table(Fd,Cs,_N,0,XRef) ->
    load_xref_table(Fd,Cs,XRef);
load_xref_table(Fd,Cs,N,I,XRef) ->
    {<<Ns:10/binary,$\s,Gs:5/binary,$\s,Key,NL:2/binary>>,Cs1} = 
	get_data(Cs,20),
    Offset = binary_to_integer(Ns),
    G = binary_to_integer(Gs),
    if NL =:= <<"\s\r">>; 
       NL =:= <<"\s\n">>;
       NL =:= <<"\r\n">> ->
	    case Key of
		$n ->
		    load_xref_table(Fd, Cs1, N+1, I-1,
				    add_xref_entry(?objref(N,G),Offset,XRef));
		$f ->
		    load_xref_table(Fd, Cs1, N+1, I-1,
				    add_xref_entry(?freeref(N,G),Offset,XRef))
	    end
    end.

add_xref_entry(Key, Value, XRef) ->
    ?xdbg("ADD XREF Entry: ~p => ~p\n", [Key,Value]),
    XRef#{ Key => Value }.

%%
%% retrive pdf info and decode into "readable" form (if possible)
%%
-spec info(PDF::pdf()) -> [{Key::atom(), Value::term()}].

info(PDF) ->
    Trailer = get_trailer(PDF),
    Info    = get_named_value('Info',Trailer,#{}),
    ?object(_,_,IObj) = get_object(Info, PDF),
    info_(IObj, PDF).

info_(Info, PDF) ->
    get_string('Title', title, Info, PDF) ++
	get_string('Author', author, Info, PDF) ++
	get_string('Subject', subject, Info, PDF) ++
	get_string('Keywords', keywords, Info, PDF) ++
	get_string('Creator', creator, Info, PDF) ++
	get_string('Producer', producer, Info, PDF) ++
	get_date('CreationDate', create_date, Info, PDF) ++
	get_date('ModDate', mod_date, Info, PDF) ++
	get_name('Trapped', trapped, Info).

%% Extract list of pages objects
-spec pages(PDF::pdf()) -> [pdf_objref()].

pages(PDF) ->
    Trailer = get_trailer(PDF),
    Root    = get_named_object('Root', Trailer, PDF),
    'Catalog' = get_named_value('Type', Root), %% check catalog
    PagesObj  = get_named_object('Pages', Root, PDF),
    'Pages'   = get_named_value('Type', PagesObj), %% check catalog
    Kids      = get_named_object('Kids',PagesObj, PDF),
    array_to_list(Kids).

%% Extract document tree object structure
-spec document_tree(PDF::pdf()) -> pdf_tree().
document_tree(PDF) ->
    Trailer   = get_trailer(PDF),
    RootRef   = maps:get('Root', Trailer),
    RootObj   = get_object(RootRef, PDF),
    PagesObj  = get_named_object('Pages', RootObj, PDF),
    Kids      = get_named_object('Kids',PagesObj, PDF),
    {RootRef,
     lists:foldl(
       fun(Page, Acc) ->
	       PageObj = maps:get(Page, PDF),
	       Contents =
		   case get_named_value('Contents', PageObj, []) of
		       Ref = ?objref(_N,_G) -> [Ref];
		       RefList when is_tuple(RefList) -> array_to_list(RefList)
		   end,
	       [{Page,Contents}|Acc]
       end, [], array_to_list(Kids))}.

%% extract text from pages
-spec text(PDF::pdf()) -> string().

text(PDF) ->
    Pages = pages(PDF),
    Text = lists:foldr(
	     fun(Page, Text) ->
		     [text_from_page(Page, PDF) | Text]
	     end, [], Pages),
    lists:flatten(Text).

%% extract text from page
-spec text_from_page(Page::pdf_objref(), PDF::pdf()) -> string().
text_from_page(Page, PDF) when ?is_objref(Page) ->
    PageObj = maps:get(Page, PDF),
    case get_named_value('Type', PageObj) of
	'Page' ->
	    Contents = case get_named_value('Contents',PageObj,{}) of
			   Cs when is_tuple(Cs) -> tuple_to_list(Cs);
			   Ref = ?objref(_N,_G) -> [Ref]
		       end,
	    Text = lists:foldr(
		     fun(Content,Text) ->
			     [text_from_content(Content, PDF) | Text]
		     end, [], Contents),
	    ascii(lists:flatten(Text));
	'Pages' ->
	    Kids = tuple_to_list(get_named_value('Kids', PageObj, {})),
	    Text = lists:foldr(
		     fun(P,Text) ->
			     [text_from_page(P, PDF) | Text]
		     end, [], Kids),
	    ascii(lists:flatten(Text))
    end.

ascii(Text) ->
    [min(126,max(32,(C band 127))) || C <- Text].

text_from_content(Content, PDF) ->
    StreamObj = maps:get(Content, PDF),
    [obj,_N,_G,[stream,Dict,Data]] = StreamObj,
    case maps:get('Filter',Dict,[]) of
	[] ->
	    if is_binary(Data) ->
		    {Text,_} = parse_seq_stream(binary_to_list(Data)),
		    text_stream(Text);
	       is_list(Data) ->
		    text_stream(Data)
	    end;
	_Fs ->
	    ?dbg1("text_from_content ~p not encoded ~p remain\n",
		  [Content,_Fs]),
	    []
    end.

text_stream([{"Tj",[String]}|Commands]) ->
    [String | text_stream(Commands)];
text_stream([{"'",[String]}|Commands]) ->
    [String | text_stream(Commands)];
text_stream([{"\"",[_Aw,_Ac,String]}|Commands]) ->
    [String | text_stream(Commands)];
text_stream([{"TJ",[Array]}|Commands]) ->
    [lists:foldr(
      fun(Text,Acc) when is_list(Text) ->
	      [Text|Acc];
	 (_, Acc) ->
	      Acc
      end, [], array_to_list(Array)) | text_stream(Commands)];
text_stream([_|Commands]) ->
    text_stream(Commands);
text_stream([]) ->
    [].

get_object(N, PDF) when is_integer(N) ->
    maps:get(?objref(N), PDF);
get_object(Ref=?objref(_N,_G), PDF) ->
    maps:get(Ref, PDF);
get_object(Obj=?object(_N,_G,_), _PDF) ->
    Obj.

get_object(N, G, PDF) when is_integer(N), is_integer(G) ->
    maps:get(?objref(N,G), PDF);
get_object(N, PDF, Default) when is_integer(N) ->
    maps:get(?objref(N), PDF, Default);
get_object(Ref=?objref(_N,_G), PDF, Default) ->
    maps:get(Ref, PDF, Default);
get_object(?object(_N,_G,Obj), _PDF, _Default) ->
    Obj.

get_object(N, G, PDF, Default) when is_integer(N) ->
    maps:get(?objref(N,G), PDF, Default).

get_named_object(Name, Dict, PDF) ->
    case get_named_value(Name, Dict) of
	Ref = ?objref(_N,_G) ->
	    maps:get(Ref,PDF);
	Obj -> Obj
    end.

%% get dictionary value
get_named_value(Name, Dict) when is_map(Dict) ->
    maps:get(Name, Dict);
get_named_value(Name, [obj,_N,_G,Dict]) when is_map(Dict) ->
    maps:get(Name, Dict);
get_named_value(Name, [obj,_N,_G,[stream,Dict,_]]) when is_map(Dict) ->
    maps:get(Name, Dict).

%% get dictionary value
get_named_value(Name, [obj,_N,_G,Dict], Default) when is_map(Dict) ->
    maps:get(Name, Dict, Default);
get_named_value(Name, [obj,_N,_G,[stream,Dict,_]], Default) when is_map(Dict) ->
    maps:get(Name, Dict, Default);
get_named_value(Name, Dict, Default) when is_map(Dict) ->
    maps:get(Name, Dict, Default).

array_to_list(Array) ->
    tuple_to_list(Array).

append_array(Array1, Array2) when is_tuple(Array1), is_tuple(Array2) ->
    list_to_tuple(tuple_to_list(Array1) ++ tuple_to_list(Array2));
append_array(Elem, Array) when is_tuple(Array) ->
    list_to_tuple([Elem | tuple_to_list(Array)]);
append_array({}, Elem) when ?is_objref(Elem) -> Elem;
append_array(Array, Elem) when is_tuple(Array) ->
    erlang:append_element(Array, Elem);
append_array(Elem1, Elem2) ->
    {Elem1,Elem2}.
    
get_string(InfoKey, Key, Info, PDF) ->
    case maps:get(InfoKey, Info, undefined) of
	undefined -> [];
	Ref = ?objref(N,G) ->
	    [obj,N,G,String] = get_object(Ref, PDF),
	    [{Key, decode_string(String)}];
	String -> [{Key, decode_string(String)}]
    end.

get_date(InfoKey, Key, Info, PDF) ->
    case maps:get(InfoKey, Info, undefined) of
	undefined -> [];
	Ref = ?objref(N,G) ->
	    [obj,N,G,String] = get_object(Ref, PDF),
	    [{Key, decode_date(Key,String)}];
	String ->
	    decode_date(Key,String)
    end.

decode_date(Key,[$D,$:,Y1,Y2,Y3,Y4|Cs]) ->
    Ds = scan_date(Cs, [month,day,hour,minute,second,z,zh,zm],
		   [{year,list_to_integer([Y1,Y2,Y3,Y4])}]),
    Date = items(Ds),
    [{Key,Date}];
decode_date(Key,SomeRandomDateString) ->
    %% FIXME: try repair?
    [{Key,SomeRandomDateString}].
    
get_name(InfoKey, Key, Info) ->
    case maps:get(InfoKey, Info, undefined) of
	undefined -> [];
	Name when is_atom(Name) -> [{Key,Name}];
	_ -> []
    end.

items(Ds) ->
    {Date, Ds1} = date_items(Ds),
    {Time, Ds2} = time_items(Ds1),
    {_Zone, _Ds3} = zone_items(Ds2),
    {Date,Time}.

date_items([{year,Y}]) -> {{Y,1,1},[]};
date_items([{year,Y},{month,M}]) -> {{Y,M,1},[]};
date_items([{year,Y},{month,M},{day,D}|Ds]) -> {{Y,M,D},Ds}.

time_items([]) -> {{0,0,0}, []};
time_items([{hour,H}]) -> {{H,0,0}, []};
time_items([{hour,H},{minute,M}]) -> {{H,M,0},[]};
time_items([{hour,H},{minute,M},{second,S}|Ds]) -> {{H,M,S},Ds}.

zone_items([]) -> {0,[]};
zone_items([{z,0}]) -> {0,[]};
zone_items([{z,Z},{zh,H}]) -> {Z*50*H,[]};
zone_items([{z,Z},{zh,H},{zm,M}|Ds]) -> {Z*(60*H+M),Ds}.
    

scan_date([M1,M2|Date], [month|Ds], Acc) ->
    scan_date(Date, Ds, [{month,list_to_integer([M1,M2])}|Acc]);
scan_date([D1,D2|Date], [day|Ds], Acc) ->
    scan_date(Date, Ds, [{day,list_to_integer([D1,D2])}|Acc]);
scan_date([H1,H2|Date], [hour|Ds], Acc) ->
    scan_date(Date, Ds, [{hour,list_to_integer([H1,H2])}|Acc]);
scan_date([M1,M2|Date], [minute|Ds], Acc) ->
    scan_date(Date, Ds, [{minute,list_to_integer([M1,M2])}|Acc]);
scan_date([S1,S2|Date], [second|Ds], Acc) ->
    scan_date(Date, Ds, [{second,list_to_integer([S1,S2])}|Acc]);
scan_date([$Z|Date], [z|Ds], Acc) -> scan_date(Date, Ds, [{z,0}|Acc]);
scan_date([$+|Date], [z|Ds], Acc) -> scan_date(Date, Ds, [{z,1}|Acc]);
scan_date([$-|Date], [z|Ds], Acc) -> scan_date(Date, Ds, [{z,-1}|Acc]);
scan_date([H1,H2|Date], [zh|Ds], Acc) ->
    scan_date(Date, Ds, [{zh,list_to_integer([H1,H2])}|Acc]);
scan_date([$',M1,M2|_Date], [zm], Acc) ->
    lists:reverse([{zm,list_to_integer([M1,M2])}|Acc]);
scan_date([], _, Acc) ->
    lists:reverse(Acc).

font_list() ->
    ['Times-Roman', 'Helvetica', 'Courier', 'Symbol', 'Times-Bold',
     'Helvetica-Bold', 'Courier-Bold', 'ZapfDingbats', 'Times-Italic',
     'Helvetica-Oblique', 'Courier-Oblique', 
     'Times-BoldItalic', 'Helvetica-BoldOblique', 'Courier-BoldOblique'].

get_char_fd_fun(Fd) ->
    fun Read()  ->
	    case file:read(Fd, 1) of
		{ok,[C]} -> 
		    %% io:format("[char] ~p (~w)\n", [[C],C]),
		    [C|Read];
		eof -> eof
	    end
    end.

get_char([]) -> eof;
get_char(<<>>) -> eof;
get_char(Read) when is_function(Read) -> Read();
get_char(<<C,Binary/binary>>) -> [C|Binary];
get_char(Cs) -> Cs.

%% get next char after wite space
get_next_char(eof) -> eof;
get_next_char(Cs) ->
    case get_char(Cs) of
	[0|Cs1]   -> get_next_char(Cs1);
	[$\t|Cs1] -> get_next_char(Cs1);
	[$\n|Cs1] -> get_next_char(Cs1);
	[$\f|Cs1] -> get_next_char(Cs1);
	[$\r|Cs1] -> get_next_char(Cs1);
	[$\s|Cs1] -> get_next_char(Cs1);
	[$%|Cs1]  -> get_next_char(skip_comment(Cs1));
	Cs1 -> Cs1
    end.

skip_newline(Cs) ->
    case get_char(Cs) of
	[$\n|Cs1] -> skip_newline(Cs1);
	[$\r|Cs1] -> skip_newline(Cs1);
	Cs1 -> Cs1
    end.

skip_white_space(Cs) ->
    case get_char(Cs) of
	Cs0=[C|Cs1] ->
	    case is_white_space(C) of
		true -> skip_white_space(Cs1);
		false when is_binary(Cs) -> Cs;
		false -> Cs0
	    end;
	Cs0 -> Cs0
    end.

%% FIXME: read more (binary?) data via get_char/get_chars?
get_data(Cs, 0) ->  {<<>>, Cs};
get_data(Cs, N) ->  get_data(Cs,N,[]).

get_data(Cs, 0, A) -> {iolist_to_binary(lists:reverse(A)),Cs};
get_data(Cs, I, A) ->
    case get_char(Cs) of
	[C|Cs1] -> get_data(Cs1,I-1,[C|A]);
	eof -> {iolist_to_binary(lists:reverse(A)),eof}
    end.

unget_chars([C|Cs], eof) ->
    unget_chars(Cs, [C]);
unget_chars([C|Cs], Cs1) ->
    unget_chars(Cs, [C|Cs1]);
unget_chars([], Cs1) ->
    Cs1.

skip_comment(Cs) ->
    case get_char(Cs) of
	Cs0 = [$\n|_] -> Cs0;
	[$\r|Cs1] ->
	    case get_char(Cs1) of
		Cs0 = [$\n|Cs1] -> Cs0;
		eof -> eof;
		Cs0 -> skip_comment(Cs0)
	    end;
	[_|Cs1] -> skip_comment(Cs1);
	eof -> eof
    end.

%% like get next char after wite space but collect chars as well
%% (to be able to unget char, from stream)
collect_next_char(eof,A) -> {eof,A};
collect_next_char(Cs,A) ->
    case get_char(Cs) of
	[0|Cs1]   -> collect_next_char(Cs1,[0|A]);
	[$\t|Cs1] -> collect_next_char(Cs1,[$\t|A]);
	[$\n|Cs1] -> collect_next_char(Cs1,[$\n|A]);
	[$\f|Cs1] -> collect_next_char(Cs1,[$\f|A]);
	[$\r|Cs1] -> collect_next_char(Cs1,[$\r|A]);
	[$\s|Cs1] -> collect_next_char(Cs1,[$\s|A]);
	[$%|Cs1]  ->
	    {Cs2,A2} = collect_comment(Cs1,[$%|A]),
	    collect_next_char(Cs2,A2);
	Cs1 -> {Cs1,A}
    end.

collect_comment(Cs,A) ->
    case get_char(Cs) of
	Cs0 = [$\n|_] -> {Cs0,A};
	[$\r|Cs1] ->
	    case get_char(Cs1) of
		Cs0 = [$\n|Cs1] -> {[$\r|Cs0],A};
		eof -> {eof,A};
		Cs0 -> collect_comment(Cs0,[$\r|A])
	    end;
	[C|Cs1] -> collect_comment(Cs1,[C|A]);
	eof -> {eof,A}
    end.

%% parse ws* unsigned
collect_unsigned(Cs,A) ->
    {Cs1,A1} = collect_next_char(Cs,A),
    case get_char(Cs1) of
	[C|Cs2] when ?is_digit(C) ->
	    collect_unsigned(Cs2,(C-$0),[C|A1]);
	Cs2 ->
	    {false,unget_chars(A,Cs2)}
    end.

collect_unsigned(Cs,N,A) ->
    case get_char(Cs) of
	[C|Cs1] when ?is_digit(C) ->
	    collect_unsigned(Cs1,N*10+(C-$0),[C|A]);
	Cs1 ->
	    {true,N,Cs1,A}
    end.

%% parse indirect object def
parse_indirect_object(Fd,Read,PDF) ->
    Cs1 = get_next_char(Read),
    {N,Cs2} = parse_unsigned(Cs1),
    parse_indirect_object2(Fd,Cs2,N,PDF).

parse_indirect_object2(Fd,Cs2,N,PDF) ->
    Cs3 = get_next_char(Cs2),
    {G,Cs4} = parse_unsigned(Cs3),
    ?dbg("loading ~w ~w\n", [N,G]),
    Cs5 = get_next_char(Cs4),
    case parse_keyword(Cs5) of
	{?obj,Cs6} ->
	    case parse_direct_object(Cs6) of
		{?endobj,Cs7} ->
		    %% null object?
		    {?object(N,G,?null), Cs7, PDF};
		{Dict,Cs7} ->
		    ?dbg("Dict ~w\n", [Dict]),
		    parse_indirect_object3(Fd,Cs7,N,G,Dict,PDF)
	    end
    end.

parse_indirect_object3(Fd,Cs,N,G,Dict,PDF) ->
    Cs1 = get_next_char(Cs),
    case parse_keyword(Cs1) of
	{?stream,Cs2} ->
	    Cs3 = skip_newline(Cs2),
	    ?dbg("stream\n", []),
	    case maps:get('Length',Dict,undefined) of
		undefined -> 
		    {Data,Cs4} = parse_seq_stream(Cs3),
		    ?dbg("Stream Data=~p\n", [Data]),
		    Cs5 = get_next_char(Cs4),
		    {?endobj,Cs6} = parse_keyword(Cs5),
		    Obj = ?object(N,G,?stream(Dict,Data)),
		    PDF1 = PDF#{ ?objref(N,G) => Obj },
		    {Obj,Cs6,PDF1};
		LRef = ?objref(_LN,_LG) ->
		    {?object(_,_,Length),PDF1} = load_object(Fd, LRef, PDF),
		    {Dict1,Data,Cs4} = load_stream_data(Cs3, Dict, Length),
		    Obj = ?object(N,G,?stream(Dict1,Data)),
		    PDF2 = PDF1#{ ?objref(N,G) => Obj },
		    {Obj,Cs4,PDF2};
		Length ->
		    {Dict1,Data,Cs4} = load_stream_data(Cs3, Dict, Length),
		    Obj = ?object(N,G,?stream(Dict1,Data)),
		    PDF1 = PDF#{ ?objref(N,G) => Obj },
		    {Obj,Cs4,PDF1}
	    end;
	{?endobj,Cs9} ->
	    Obj = ?object(N,G,Dict),
	    PDF1 = PDF#{ ?objref(N,G) => Obj },	    
	    {Obj,Cs9,PDF1}
    end.

%% load stream data and match enstream + endobj
load_stream_data(Cs, Dict, Length) when is_integer(Length) ->
    {Data,Cs1} = get_data(Cs, Length),
    ?dbg("Stream Data [Length=~w] = ~p\n", [Length, Data]),
    {Dict1,Data1} = decode_stream(Dict, Data),
    ?dbg("Decoded stream data [Length=~w] = ~p\n", [byte_size(Data1),Data1]),
    Cs2 = get_next_char(Cs1),
    {?endstream,Cs3} = parse_keyword(Cs2),
    Cs4 = get_next_char(Cs3),
    {?endobj,Cs5} = parse_keyword(Cs4),
    %% {Data2,_Cs10} = parse_seq_stream(Data1),    
    {Dict1,Data1,Cs5}.

%% parse command stream: [<obj>* <cmd>]*
parse_seq_stream(Cs) ->
    ?dbg0("parse_seq_stream: ~p\n", [Cs]),
    parse_seq_stream_(Cs, []).

parse_seq_stream_(Cs, A) ->
    case parse_args(Cs,[]) of
	{Cmd, Args, Cs1} ->
	    ?dbg0("command ~p\n", [{Cmd,Args}]),
	    parse_seq_stream_(Cs1,[{Cmd,Args}|A]);
	eof ->
	    {lists:reverse(A),eof};
	{?endstream, Cs1} ->
	    {lists:reverse(A),Cs1}
    end.

parse_args(Cs, A) ->
    case parse_direct_object(Cs) of
	{?endstream, Cs1} -> {?endstream, Cs1};
	{?key(Cmd), Cs1} -> {Cmd,lists:reverse(A),Cs1};
	{Arg,Cs1} -> parse_args(Cs1, [Arg|A]);
	eof -> eof
    end.

%% parse direct object / object reference
parse_direct_object(Cs) ->
    case parse_objref(Cs) of
	{true,ObjRef,Cs1} ->
	    {ObjRef,Cs1};
	{false,Cs1} ->
	    parse_obj(Cs1)
    end.

%% try parse objref: <unsigned> <unsigned> 'R'
parse_objref(Cs) ->
    A0 = [],
    case collect_unsigned(Cs, A0) of
	{true,N,Cs1,A1} ->
	    case collect_unsigned(Cs1,A1) of
		{true,G,Cs2,A2} ->
		    case collect_next_char(Cs2,A2) of
			{[$R|Cs3],A3} ->
			    case get_char(Cs3) of
				Cs4 = [C|_] ->
				    case is_white_space(C) orelse 
					is_delimiter(C) of
					true -> 
					    {true,?objref(N,G),Cs4};
					false ->
					    Cs5 = unget_chars([C,$R|A3],Cs4),
					    {false,Cs5}
				    end;
				eof ->
				    {true,?objref(N,G),Cs3}
			    end;
			{Cs3,A3} ->
			    {false,unget_chars(A3,Cs3)}
		    end;
		R={false,_} -> R
	    end;
	R={false,_} -> R
    end.

parse_obj(Cs) ->
    case parse_obj_(Cs) of
	eof -> eof;
	{Obj,Cs1} -> {Obj,Cs1}
    end.

parse_obj_(Cs) ->
    case get_next_char(Cs) of
	eof -> eof;
	[$.|Cs1] -> parse_number(Cs1, true, [$.]);
	[$-|Cs1] -> parse_number(Cs1, false, [$-]);
	[$+|Cs1] -> parse_number(Cs1, false, [$+]);
	[$(|Cs1] -> parse_string(Cs1, [], 0);
	[$<|Cs1] ->
	    case get_char(Cs1) of
		[$<|Cs2] -> parse_dict(Cs2, []);
		[C|Cs2] when ?is_xdigit(C) -> parse_hex_string(Cs2,C,[])
	    end;
	[$[|Cs1] -> parse_array(Cs1, []);
	[$/|Cs1] -> parse_name(Cs1, []);
	[C|Cs1] when ?is_digit(C) -> parse_number(Cs1, false, [C]);
	[C|Cs1] when ?is_keyword1(C) -> parse_keyword(Cs1, [C])
    end.

parse_keyword(Cs) ->
    parse_keyword(Cs,[]).

parse_keyword(Cs, A) ->
    case get_char(Cs) of
	[C|Cs1] when ?is_keyword2(C) ->
	    parse_keyword(Cs1, [C|A]);
	Cs0=[C|Cs1] ->
	    case is_white_space(C) of
		true ->
		    match_keyword(lists:reverse(A), Cs1);
		false ->
		    case is_delimiter(C) of
			true ->
			    match_keyword(lists:reverse(A), Cs0)
		    end
	    end;
	eof ->
	    match_keyword(lists:reverse(A), eof)
    end.

match_keyword(Key, Cs) ->
    case Key of
	"R"    -> {?R,Cs};
	"true" -> {true, Cs};
	"false" -> {false,Cs};
	"null" -> {?null,Cs};
	"obj" -> {?obj,Cs};
	"endobj" -> {?endobj,Cs};
	"stream" -> {?stream,Cs};
	"endstream" -> {?endstream,Cs};
	"xref" -> {?key(xref),Cs};
	"startxref" -> {?key(startxref),Cs};
	"trailer" -> {?key(trailer),Cs};
	_ -> {?key(Key),Cs}
    end.

%% util and test
parse_array(Cs) ->
    case get_char(Cs) of
	[$[|Cs1] -> parse_array(Cs1,[]);
	Cs1 -> {error, Cs1}
    end.

parse_array(Cs, Acc) ->
    case get_next_char(Cs) of
	[$]|Cs1] -> 
	    {list_to_tuple(lists:reverse(Acc)),Cs1};
	Cs1 ->
	    {Elem,Cs2} = parse_direct_object(Cs1),
	    parse_array(Cs2, [Elem|Acc])
    end.

parse_dict(Cs) ->
    case get_next_char(Cs) of
	[$<|Cs1] ->
	    case get_char(Cs1) of
		[$<|Cs2] -> parse_dict(Cs2, [])
	    end
    end.

parse_dict(Cs, Acc) ->
    ?dbg0("parse_dict: Cs=~p\n", [Cs]),
    case get_next_char(Cs) of
	[$>|Cs1] -> 
	    [$>|Cs2] = get_char(Cs1),
	    {maps:from_list(Acc),Cs2};
	[$/|Cs1] ->
	    {Key,Cs2} = parse_name(Cs1,[]),
	    Cs3 = get_next_char(Cs2),
	    {Value,Cs4} = parse_direct_object(Cs3),
	    ?dbg0("Key=~p, Value=~p\n", [Key,Value]),
	    parse_dict(Cs4, [{Key,Value}|Acc])
    end.

parse_string(Cs) ->
    parse_string(Cs, [], 0).

parse_string(Cs, A, L) ->
    case get_char(Cs) of
	[$)|Cs1] when L =:= 0 ->
	    {lists:reverse(A),Cs1};
	[$\\|Cs1] ->
	    case get_char(Cs1) of
		[$n|Cs2] -> parse_string(Cs2,[$\n|A],L);
		[$r|Cs2] -> parse_string(Cs2,[$\r|A],L);
		[$t|Cs2] -> parse_string(Cs2,[$\t|A],L);
		[$b|Cs2] -> parse_string(Cs2,[$\b|A],L);
		[$f|Cs2] -> parse_string(Cs2,[$\f|A],L);
		[$(|Cs2] -> parse_string(Cs2,[$(|A],L);
		[$)|Cs2] -> parse_string(Cs2,[$)|A],L);
		[$\\|Cs2] -> parse_string(Cs2,[$\\|A],L);
		[D1|Cs2] when ?is_octal(D1) ->
		    case get_char(Cs2) of
			[D2|Cs3] when ?is_octal(D2) ->
			    case get_char(Cs3) of
				[D3|Cs4] when ?is_octal(D3) ->
				    X = ((D1-$0)*64 + (D2-$0)*8 + (D3-$0)),
				    parse_string(Cs4,[X band 16#ff|A],L);
				Cs4 ->
				    X = ((D1-$0)*8 + (D2-$0)),
				    parse_string(Cs4,[X|A],L)
			    end;
			Cs3 ->
			    X = (D1-$0),
			    parse_string(Cs3,[X|A],L)
		    end
	    end;
	[$(|Cs1] -> parse_string(Cs1,[$(|A],L+1);
	[$)|Cs1] -> parse_string(Cs1,[$)|A],L-1);
	[C|Cs1]   -> parse_string(Cs1,[C|A],L)
    end.

parse_hex_string(Cs, A) ->
    case get_next_char(Cs) of
	[C|Cs1] when ?is_xdigit(C) -> parse_hex_string(Cs1,C,A);
	[$>|Cs1] -> {lists:reverse(A),Cs1}
    end.

parse_hex_string(Cs,X,A) ->
    case get_next_char(Cs) of
	[C|Cs1] when ?is_xdigit(C) ->
	    parse_hex_string(Cs1,[list_to_integer([X,C],16)|A]);
	[$>|Cs1] -> 
	    {lists:reverse([list_to_integer([X,$0],16)|A]), Cs1}
    end.

is_white_space(C) ->
    maps:get(C, #{ 0  => true, $\t => true, $\n => true,
		   $\f => true, $\r => true, $\s => true }, false).

is_delimiter(C) ->
    maps:get(C, #{ $( => true, $) => true,
		   $< => true, $> => true,
		   $[ => true, $] => true,
		   ${ => true, $} => true,
		   $% => true, $/ => true
		 },
	     false).

parse_name(Cs) -> parse_name(Cs,[]).
parse_name(Cs, A) ->
    case get_char(Cs) of
	[$#|Cs1] ->
	    [X1|Cs2] = get_char(Cs1),
	    [X2|Cs3] = get_char(Cs2),
	    parse_name(Cs3, [list_to_integer([X1,X2],16)|A]);
	Cs0=[C|Cs1] ->
	    case is_delimiter(C) of
		true -> {list_to_atom(lists:reverse(A)), Cs0};
		false when C >= $!, C =< $~ -> parse_name(Cs1, [C|A]);
		false -> {list_to_atom(lists:reverse(A)), Cs0}
	    end;
	eof ->
	    {list_to_atom(lists:reverse(A)), []}
    end.

parse_int(Cs) ->
    case get_char(Cs) of
	[$-|Cs1] -> parse_unsigned(Cs1, [$-]);
	[$+|Cs1] -> parse_unsigned(Cs1, []);
	[C|Cs1] when ?is_digit(C) -> parse_unsigned(Cs1,[C]);
	Cs1 -> {error,Cs1}
    end.

parse_unsigned(Cs) -> parse_unsigned(Cs,[]).
parse_unsigned(Cs,A) ->
    case get_char(Cs) of 
	[C|Cs1] when ?is_digit(C) ->
	    parse_unsigned(Cs1, [C|A]);
	Cs1 ->
	    try list_to_integer(lists:reverse(A)) of
		Int -> {Int,Cs1}
	    catch
		error:_ -> error
	    end
    end.


%% util fixme parse according to spec! exponents
parse_float(Cs) ->
    parse_number(Cs).

parse_number(Cs) ->
    case get_char(Cs) of
	[$.|Cs1] -> parse_number(Cs1, true, [$.]);
	[$-|Cs1] -> parse_number(Cs1, false, [$-]);
	[$+|Cs1] -> parse_number(Cs1, false, [$+]);
	[C|Cs1] when ?is_digit(C) -> parse_number(Cs1, false, [C]);
	Cs1 -> {error,Cs1}
    end.

parse_number(Cs, Dot, A) ->
    case get_char(Cs) of
	[C|Cs1] when ?is_digit(C) ->
	    parse_number(Cs1, Dot, [C|A]);
	[$.|Cs1] when not Dot ->
	    parse_number(Cs1, true, [$.|A]);
	Cs1 when not Dot ->
	    {to_int(lists:reverse(A)),Cs1};
	Cs1 when Dot ->
	    case A of
		[$.|A1] ->
		    {to_float(lists:reverse([$0,$.|A1])), Cs1};
		_ ->
		    {to_float(lists:reverse(A)), Cs1}
	    end
    end.

to_int(Ds) ->
    try list_to_integer(Ds) of
	Int -> Int
    catch
	error:_ -> error
    end.

to_float([$.|Ds0]) -> to_float_([$0,$.|Ds0]);
to_float([$-,$.|Ds0]) -> to_float_([$-,$0,$.|Ds0]);
to_float([$+,$.|Ds0]) -> to_float_([$+,$0,$.|Ds0]);
to_float(Ds0) -> to_float_(Ds0).

to_float_(Ds) ->
    try list_to_float(Ds) of
	Float -> Float
    catch
	error:_ -> error
    end.

format_obj(X) when is_integer(X) -> format_int(X);
format_obj(X) when is_float(X) -> format_float(X);
format_obj(X) when is_boolean(X) -> format_boolean(X);
format_obj(X) when is_atom(X) -> format_name(X);
format_obj(?null) -> "null";
format_obj([stream,Dict=#{},StreamData]) ->
    Data = if is_binary(StreamData) ->
		   StreamData;
	      true ->
		   format_stream_obj(StreamData)
	   end,
    Dict1 = case maps:get('Length',Dict,undefined) of
		undefined -> Dict# {'Length' => iolist_size(Data) };
		_ -> Dict
	    end,
    [format_dict(Dict1),
     ?NL,<<"stream">>,?NL,
     Data,
     ?NL,<<"endstream">>,?NL];
format_obj(?objref(N,G)) when is_integer(N),is_integer(G) ->
    [integer_to_list(N),?BL,integer_to_list(G),?BL,"R"];
format_obj(?object(N,G,Obj)) when is_integer(N),is_integer(G) ->
    [integer_to_list(N),?BL,integer_to_list(G),?BL,<<"obj">>,?NL,
     format_obj(Obj),?NL,
     <<"endobj">>,?NL];
format_obj(X) when is_list(X) -> format_string(X);
format_obj(X) when is_binary(X) -> format_hex_string(X);
format_obj(X) when is_tuple(X) -> format_array(X);
format_obj(X) when is_map(X) -> format_dict(X).

%% Param to Tr render!
-define(TEXT_FILL, 0).
-define(TEXT_STROKE, 1).
-define(TEXT_FILL_STROKE, 2).
-define(TEXT_INVISIBLE, 3).
-define(TEXT_FILL_CLIPPING, 4).
-define(TEXT_STROKE_CLIPPING, 5).
-define(TEXT_FILL_STROKE_CLIPPING, 6).
-define(TEXT_CLIPPING, 7).

%% format text operators
format_stream_obj({text,List}) ->
    ["BT",?NL,[[?BL,text_operator(Op)] || Op <- List],?NL,"ET",?NL];

format_stream_obj({graphics,List}) ->
    [[[?BL,graphics_operator(Op)] || Op <- List],?NL].

%% Fixme: give nice name for the various operators
%% and fall back to the general scheme with internal variants
text_operator(Op) ->
    case Op of
	{"Td",[Tx,Ty]} ->
	    [format_number(Tx),?BL,format_number(Ty),?BL,"Td"];
	{"TD",[Tx,Ty]} ->
	    [format_number(Tx),?BL,format_number(Ty),?BL,"TD"];
	{"Tj",[String]} ->
	    [format_string(String),?BL,"Tj"];
	{"'",[String]} ->  %% T* (string) Tj
	    [format_string(String),?BL,"'"];
	{"\"",[Aw,Ac,String]} ->
	    [format_number(Aw),?BL,format_number(Ac),?BL,
	     format_string(String),?BL,"\""];
	{"Tm",[A,B,C,D,E,F]} ->
	    [format_number(A),?BL,format_number(B),?BL,
	     format_number(C),?BL,format_number(D),?BL,
	     format_number(E),?BL,format_number(F),?BL,"Tm"];
	{"T*",[]} ->
	    ["T*"];
	{"TJ",[Array]} ->
	    [format_array(Array),?BL,"TJ"];
	{"Tc",[CharSpace]} ->
	    [format_number(CharSpace),?BL,"Tc"];
	{"Tw",[WordSpace]} ->
	    [format_number(WordSpace),?BL,"Tw"];
	{"Tz",[Scale]} ->
	    [format_number(Scale),?BL,"Tz"];
	{"TL",[Leading]} ->
	    [format_number(Leading),?BL,"TL"];
	{"Tf",[Font,Size]} ->
	    [format_name(Font),?BL,format_number(Size),?BL,"Tf"];
	{"Tr",[Render]} ->
	    [format_int(Render),?BL,"Tr"];
	{"Ts",[Rise]} ->
	    [format_number(Rise),?BL,"Ts"];
	{"Tk",[Knockout]} ->
	    [format_boolean(Knockout),?BL,"Tk"];
	Name when is_list(Name) ->
	    Name;
	{Name,Operands} when is_list(Name) ->
	    %% fixme warning...
	    [format_list(Operands),?BL,Name]
    end.

graphics_operator(Op) ->
    case Op of
	Name when is_list(Name) ->
	    atom_to_list(Name);
	{Name,Operands} when is_list(Name) ->
	    %% fixme warning...
	    [format_list(Operands),?BL,Name]
    end.

format_list(Ts) ->
    format_list(Ts,$\s).

format_list([T],_Sep) -> format_obj(T);
format_list([T|Ts],Sep) -> [format_obj(T),Sep | format_list(Ts,Sep)];
format_list([],_Sep) -> [].

format_number(X) when is_integer(X) -> format_int(X);
format_number(X) when is_float(X) -> format_float(X).

format_float(X) ->
    io_lib_format:fwrite_g(X).

format_int(X) ->
    integer_to_list(X).

format_boolean(true) -> "true";
format_boolean(false) -> "false".

format_name(X) ->
    [$/ | [format_name_char(Y) || Y <- atom_to_list(X)]].

%% literal string (char*)
format_string(X) ->
    ["(", [format_string_char(Y) || Y <- X], ")"].

%% literal string <hex>
format_hex_string(X) ->
    ["<", [format_hex2(Y) || <<Y>> <= X], ">"].

format_array(X) ->
    ["[", format_list(tuple_to_list(X)), "]"].

format_dict(X) ->
    Items = maps:fold(fun(K,V,Acc) -> [K,V|Acc] end, [], X),
    ["<<", format_list(Items), ">>"].

format_string_char(Y) ->
    case Y of
	$\n -> "\\n";
	$\r -> "\\r";
	$\t -> "\\t";
	$\b -> "\\b";
	$\f -> "\\f";
	$( -> "\\(";
	$) -> "\\)";
	$\\ -> "\\";
	_ when Y < $\s; Y > $~ ->
	    [$\\,format_octal3(Y)];
	_ ->
	    Y
    end.

format_name_char(Y) ->
    if Y =< $\s; Y > $~ ->
	    [$# | format_hex2(Y)];
       true ->
	    Y
    end.

format_octal3(X) ->
    tl(integer_to_list(8#1000+X,8)).

format_hex2(X) when ?is_byte(X) ->
    tl(integer_to_list(16#100+X,16)).

%% convert UTF encoded string to list
%% useful to decode 'Producer' / 'Author' / 'Creator' etc
decode_string(String) ->
    Binary = iolist_to_binary(String),
    case unicode:bom_to_encoding(Binary) of
	{_Encoding,0} -> 
	    String;
	{Encoding,N} ->
	    <<_:N/binary, Binary1/binary>> = Binary,
	    unicode:characters_to_list(Binary1, Encoding)
    end.

decode_stream(Dict, Data) ->
    Filter = case maps:get('Filter', Dict, {}) of
		 F when is_atom(F) -> [F];
		 Fs -> tuple_to_list(Fs)
	     end,
    Parms = case maps:get('DecodeParms', Dict, empty_parms(length(Filter))) of
		D when is_map(D) -> [D];
		Ds -> tuple_to_list(Ds)
	    end,
    ?dbg("decode_stream: Filter=~p, Parms=~p\n", [Filter,Parms]),
    case decode_stream_(Filter, Parms, Data) of
	{[],_,Data1} ->
	    Dict1 = maps:remove('Filter', Dict),
	    Dict2 = maps:remove('DecodeParms', Dict1),
	    {Dict2, Data1};
	{Fs1,Ps1,Data1} ->
	    {Dict#{ 'Filter' => list_to_tuple(Fs1),
		    'DecodeParms' => list_to_tuple(Ps1)
		  }, Data1}
    end.

empty_parms(N) ->
    list_to_tuple(lists:duplicate(N, #{})).

decode_stream_(Fs0=[F|Fs], Ps0=[Dict|Parms], Data) ->
    case filter_decode(F,Dict,Data) of
	{false,Data1} ->
	    {Fs0, Ps0, Data1};
	{true,Data1} ->
	    decode_stream_(Fs, Parms, Data1)
    end;
decode_stream_([], [], Data) ->
    {[],[],Data}.

encode_stream(Dict, Data) ->
    Filter = case maps:get('Filter', Dict, {}) of
		 F when is_atom(F) -> [F];
		 Fs -> lists:reverse(tuple_to_list(Fs))
	     end,
    lists:foldl(fun filter_encode/2, Data, lists:reverse(Filter)).

%% filter dispatch    
filter_encode('ASCIIHexDecode', Data) ->
    filter_ASCIIHexEncode(Data);
filter_encode('ASCII85Decode', Data) ->
    filter_ASCII85Encode(Data);
filter_encode('FlateDecode', Data) ->
    filter_FlateEncode(Data);
filter_encode('LZWDecode', Data) ->
    filter_LZWEncode(Data).

filter_decode('ASCIIHexDecode',Dict,Data) ->
    filter_ASCIIHexDecode(Data,Dict);
filter_decode('ASCII85Decode',Dict, Data) ->
    filter_ASCII85Decode(Data,Dict);
filter_decode('FlateDecode',Dict, Data) ->
    filter_FlateDecode(Data,Dict);
filter_decode('LZWDecode',Dict, Data) ->
    filter_LZWDecode(Data,Dict);
filter_decode(_Codec, _Dict, _Data) ->
    ?dbg1("filter_decode codec=~p not implemented (yet)\n", [_Codec]),
    {false, _Data}.

filter_ASCIIHexEncode(Data) ->
    iolist_to_binary([[format_hex2(X) || 
			  <<X>> <= iolist_to_binary(Data)],$>]).

filter_ASCIIHexDecode(Binary,_Dict) when is_binary(Binary) ->
    {true,iolist_to_binary(hex_decode_(Binary,[]))}.

hex_decode_(Data,A) ->
    case skip_white_space(Data) of
	<<$>,_/binary>> -> lists:reverse(A);
	<<C,Data1/binary>> when ?is_xdigit(C) -> hex_decode__(Data1,C,A)
    end.

hex_decode__(Data,X,A) ->
    case skip_white_space(Data) of
	<<$>,_/binary>> -> 
	    lists:reverse([list_to_integer([X,$0],16)|A]);
	<<C,Data1/binary>> when ?is_xdigit(C) -> 
	    hex_decode_(Data1,[list_to_integer([X,C],16)|A])
    end.
    
filter_ASCII85Encode(Data) ->
    Binary = iolist_to_binary(Data),
    Size = byte_size(Binary),
    N = Size band 16#3,
    %% Pad = (4 - N) band 16#3,  %% number of pad bytes
    Base85 = [encode_base85(B) || <<B:32>> <= Binary],
    if N =:= 0 ->
	    iolist_to_binary([Base85,$~,$>]);
       N =:= Size ->
	    <<B:32>> = <<Binary/binary,0:(4-N)/unit:8>>,
	    <<C1,C2,C3,C4,_C5>> = encode_base85_(B),
	    case 4-N of
		3 -> iolist_to_binary([C1,C2,$~,$>]);
		2 -> iolist_to_binary([C1,C2,C3,$~,$>]);
		1 -> iolist_to_binary([C1,C2,C3,C4,$~,$>])
	    end;
       true ->
	    Size4 = Size-N,
	    <<_:Size4/binary, Last/binary>> = Binary,
	    <<B:32>> = <<Last/binary,0:(4-N)/unit:8>>,
	    <<C1,C2,C3,C4,_C5>> = encode_base85_(B),
	    case 4-N of
		3 -> iolist_to_binary([Base85,C1,C2,$~,$>]);
		2 -> iolist_to_binary([Base85,C1,C2,C3,$~,$>]);
		1 -> iolist_to_binary([Base85,C1,C2,C3,C4,$~,$>])
	    end
    end.

filter_ASCII85Decode(Binary,_Dict) when is_binary(Binary) ->
    {true,iolist_to_binary(base85_decode_(Binary,[]))}.

base85_decode_(<<$z,Binary/binary>>, A) ->
    base85_decode_(Binary,[0,0,0,0 | A]);
base85_decode_(Data, A) ->
    case skip_white_space(Data) of
	<<$~,Data1/binary>>  -> 
	    base85_decode_eod(Data1,0,$u,$u,$u,$u,$u,A);
	<<C,Data1/binary>> when C >= $!, C =< $u -> 
	    base85_decode1_(Data1,C,A)
    end.

base85_decode1_(Data,C1,A) ->
    case skip_white_space(Data) of
	<<C,Data1/binary>> when C >= $!, C =< $u -> 
	    base85_decode2_(Data1,C1,C,A)
    end.

base85_decode2_(Data,C1,C2,A) ->
    case skip_white_space(Data) of
	<<$~,Data1/binary>>  -> 
	    base85_decode_eod(Data1,2,C1,C2,$u,$u,$u,A);
	<<C,Data1/binary>> when C >= $!, C =< $u -> 
	    base85_decode3_(Data1,C1,C2,C,A)
    end.

base85_decode3_(Data,C1,C2,C3,A) ->
    case skip_white_space(Data) of
	<<$~,Data1/binary>>  -> 
	    base85_decode_eod(Data1,3,C1,C2,C3,$u,$u,A);
	<<C,Data1/binary>> when C >= $!, C =< $u -> 
	    base85_decode4_(Data1,C1,C2,C3,C,A)
    end.

base85_decode4_(Data,C1,C2,C3,C4,A) ->
    case skip_white_space(Data) of
	<<$~,Data1/binary>>  -> 
	    base85_decode_eod(Data1,4,C1,C2,C3,C4,$u,A);
	<<C,Data1/binary>> when C >= $!, C =< $u -> 
	    base85_decode_(Data1, [decode_base85(C1,C2,C3,C4,C)|A])
    end.

base85_decode_eod(<<$>,_/binary>>,0,_C1,_C2,_C3,_C4,_C5,A) ->
    lists:reverse(A);
base85_decode_eod(<<$>,_/binary>>,N,C1,C2,C3,C4,C5,A) ->
    <<B1,B2,B3,_B4>> = decode_base85(C1,C2,C3,C4,C5),
    case N of
	2 -> lists:reverse([B1|A]);
	3 -> lists:reverse([B2,B1|A]);
	4 -> lists:reverse([B3,B2,B1|A])
    end.

%% convert a 32 bit unsigned number to base 85
encode_base85(0) ->
    "z";
encode_base85(X) when is_integer(X) ->
    encode_base85_(X);
encode_base85(<<B0:32>>) ->
    encode_base85_(B0).

encode_base85_(B0) ->
    C5 = B0 rem 85, B1 = B0 div 85,
    C4 = B1 rem 85, B2 = B1 div 85,
    C3 = B2 rem 85, B3 = B2 div 85,
    C2 = B3 rem 85, B4 = B3 div 85,
    C1 = B4 rem 85, C1 = B4, %% ???
    <<(C1+$!),(C2+$!),(C3+$!),(C4+$!),(C5+$!)>>.

decode_base85(<<C1,C2,C3,C4,C5>>) ->
    decode_base85(C1,C2,C3,C4,C5).
decode_base85(C1,C2,C3,C4,C5) ->
    C = ((((C1-$!)*85 + (C2-$!))*85 + (C3-$!))*85 + (C4-$!))*85 + (C5-$!),
    <<C:32>>.

filter_LZWEncode(Data) ->
    epx_lzw:compress(Data).

filter_LZWDecode(Data,_Dict) ->
    %% Fixme parameters
    {true,epx_lzw:decompress(Data)}.

filter_FlateEncode(Data) ->
    %% add pred
    zlib:compress(Data).

filter_FlateDecode(Data,Dict) ->
    %% Fixme implement various predictors
    BitsPerComponent = maps:get('BitsPerComponent',Dict, 8),
    Colors = maps:get('Colors',Dict, 1),
    Bps = BitsPerComponent*Colors, %% Bits per sample
    Columns = maps:get('Columns',Dict, 1),
    Data1 = zlib:uncompress(Data),
    Predictor = maps:get('Predictor',Dict, 1),
    ?dbg("Data1 [Predictor=~w,Length=~w] ~p\n", 
	  [Predictor, byte_size(Data1), Data1]),
    case Predictor of
	1 ->
	    {true,Data1};
	2 -> %% TIFF Predictor 2
	    {false,Data};
	10 ->
	    {true,Data1};
	    %%{true, flate_decode(Data1, Columns, Bps, 0, fun filter_none/3)};
	11 ->
	    {true, flate_decode(Data1, Columns, Bps, 1, fun filter_sub/3)};
	12 ->
	    {true, flate_decode(Data1, Columns, Bps, 2, fun filter_up/3)};
	13 ->
	    {true, flate_decode(Data1, Columns, Bps, 3, fun filter_avg/3)};
	14 ->
	    {true, flate_decode(Data1, Columns, Bps, 4, fun filter_paeth/3)};
	15 ->
	    {true, flate_decode(Data1, Columns, Bps, -1, fun filter_optimum/3)};
	_ ->
	    {false,Data}
    end.

%% Match each row with RowCode - why have this byte if is known?
%% for optimum right but... why?
flate_decode(Data, Columns, Bps, RowCode, Filter) ->
    Bpp = (Bps + 7) div 8,
    RowBytes = (Columns*Bps + 7) div 8,
    ?dbg("RowCode=~w, Columns=~w\n", [RowCode, Columns]),
    ?dbg("Bps=~w, Bpp=~w, RowBytes=~w\n", [Bps,Bpp,RowBytes]),
    Prior = <<0:RowBytes/unit:8>>,
    flate_decode(Data, RowBytes, Prior, Columns, Bpp, RowCode, Filter, []).

flate_decode(<<>>, _RowBytes, _Prior, _Columns, _Bpp, _RowCode, _Filter, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
flate_decode(Data, RowBytes, Prior, Columns, Bpp, RowCode, Filter, Acc) ->
    case Data of
	<<RowCode,Row:RowBytes/binary, Data1/binary>> ->
	    Prior1 = Filter(Row, Prior, Bpp),
	    flate_decode(Data1, RowBytes, Prior1, Columns, Bpp, RowCode, 
			 Filter, [Prior1|Acc]);
	<<Row:(RowBytes+1)/binary, Data1/binary>> when RowCode =:= -1 ->
	    Prior1 = Filter(Row, Prior, Bpp),
	    flate_decode(Data1, RowBytes, Prior1, Columns, Bpp, RowCode,
			 Filter, [Prior1|Acc])
    end.

filter_optimum(Row, Prior, Bpp) ->
    case Row of
	<<0,Row1/binary>> -> filter_none(Row1,Prior,Bpp);
	<<1,Row1/binary>> -> filter_sub(Row1,Prior,Bpp);
	<<2,Row1/binary>> -> filter_up(Row1,Prior,Bpp);
	<<3,Row1/binary>> -> filter_avg(Row1,Prior,Bpp);
	<<4,Row1/binary>> -> filter_paeth(Row1,Prior,Bpp)
    end.
    
%% Raw(x) = x
filter_none(Row, _Prior, _Bpp) ->
    Row.

%%
%% Raw(x) = Sub(x) + Raw(x-bpp)  [ Sub(x) = Raw(x) - Raw(x-bpp) ]
%%
filter_sub(Sub,_Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_sub(Sub, 0, size(Sub), [], Rn, Rm).

filter_sub(_Sub, X, X, Acc, _, _) ->
    list_to_binary(lists:reverse(Acc));
filter_sub(Sub, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Sx:8, _/binary>> = Sub,
    Rx = (Sx + Rxb) band 16#ff,
    if Rn =:= [] ->
	    filter_sub(Sub,X+1,N,[Rx|Acc],lists:reverse([Rx|Rm]),[]);
       true ->
	    filter_sub(Sub,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.
%%
%% Raw(x) = Up(x) + Prior(x) [ Up(x) = Raw(x) - Prior(x) ]
%% 
filter_up(Up, Prior, _Bpp) ->
    filter_up(Up, Prior, 0, byte_size(Up), []).
    
filter_up(_Up, _Prior, X, X, Acc) ->
    list_to_binary(lists:reverse(Acc));
filter_up(Up, Prior, X, N, Acc) ->
    <<_:X/binary,Ux:8,_/binary>> = Up,
    <<_:X/binary,Px:8,_/binary>> = Prior,
    Rx = (Ux + Px) band 16#ff,
    filter_up(Up,Prior,X+1,N,[Rx|Acc]).

%%
%% Raw(x) = Avarage(x) + floor((Raw(x-bpp)+Prior(x))/2)
%%    [ Avarage(x) = Raw(x) - floor((Raw(x-bpp)+Prior(x))/2) ]
%%

filter_avg(Avg,Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_avg(Avg, Prior, 0, byte_size(Avg), [], Rn, Rm).

filter_avg(_Avg,_Prior, X, X, Acc, _, _) ->
    list_to_binary(lists:reverse(Acc));
filter_avg(Avg, Prior, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Ax:8, _/binary>> = Avg,
    <<_:X/binary,Px:8,_/binary>> = Prior,
    Rx = (Ax + ((Rxb+Px) div 2)) band 16#ff,
    if Rn == [] ->
	    filter_avg(Avg,Prior,X+1,N,[Rx|Acc],lists:reverse([Rx|Rm]),[]);
       true ->
	    filter_avg(Avg,Prior,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.

%%
%% Paeth(x) = Raw(x) -
%%            PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
%% Raw(x) = Paeth(x) + PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
filter_paeth(Pae,Prior,Bpp) ->
    Pn = Rn = lists:duplicate(Bpp, 0),
    Pm = Rm = [],
    filter_pae(Pae, Prior, 0, byte_size(Pae), [], Rn, Rm, Pn, Pm).


filter_pae(_Pae, _Prior, X, X, Acc, _Rn, _Rm, _Pn, _Pm) ->
    list_to_binary(lists:reverse(Acc));
filter_pae(Pae, Prior, X, N, Acc, [Rxb|Rn], Rm, [Pxb|Pn], Pm) ->
    <<_:X/binary, PAx:8, _/binary>> = Pae,
    <<_:X/binary,Px:8,_/binary>> = Prior,
    Rx = (PAx + paethPredictor(Rxb, Px, Pxb)) band 16#ff,
    if Rn =:= [] ->
	    filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
		       lists:reverse([Rx|Rm]),[],
		       lists:reverse([Px|Pm]),[]);
       true ->
	    filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
		       Rn,[Rx|Rm],
		       Pn,[Px|Pm])
    end.

-define(dabs(X,Y),
	if (X) > (Y) -> (X) - (Y);
	   true -> (Y) - (X)
	end).
		
paethPredictor(A,B,C) ->
    P = A + B - C,
    PA = ?dabs(P,A),
    PB = ?dabs(P,B),
    PC = ?dabs(P,C),
    if PA =< PB, PA =< PC -> A;
       PB =< PC -> B;
       true -> C
    end.

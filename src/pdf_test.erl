%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Various tests
%%% @end
%%% Created : 11 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(pdf_test).

-compile(export_all).

-include("pdf.hrl").

test1() ->
    print_obj(17),
    print_obj(3.14),
    print_obj('Length'),
    print_obj(true),
    print_obj(false),
    print_obj("Hello world"),
    print_obj("Hello \nworld"),
    print_obj(<<1,2,3,4>>),
    print_obj(<<1,2,3>>),
    print_obj({549,3.14,false,"Ralph",'SomeName'}),
    print_obj(#{}),
    print_obj(#{ 'Length' => 100 }),
    print_obj(#{ a => 1, b => 1.0, c => "1.0", d => true }),
    print_obj(#{ a => #{ a=>1, b=> 2}}),
    %% streams / objects
    print_obj([obj,12,1,"Brillig"]),
    print_obj(?objref(12,1)),
    ok.

test2() ->
    Term = #{ 'Type' => 'Example',
	      'Subtype' => 'DictionaryExample',
	      'Version' => 0.01,
	      'IntegerItem' => 12,
	      'StringItem' => "( a string )",
	      'Subdictionary' => #{ 'Item1' => 0.4,
				    'Item2' => true,
				    'LastItem' => "not!",
				    'VeryLastItem' => "OK"
				  }
	    },
    print_obj(Term).

test3() ->
    Term = ?object(7,?stream(#{ 'Length' => ?objref(8)},
			     {text,
			      [{"Tf",['F1',12]},
			       {"Td",[72,712]},
			       {"Tj",["A stream with a indirect length"]}
			      ]})),
    print_obj(Term).

%% H.2 Minimal PDF FIle from 1.7 standard
test_doc1() ->
    PDF =
	[
	 ?object(1,
		 #{ 'Type' => 'Catalog',
		    'Outlines' => ?objref(2),
		    'Pages' => ?objref(3) }),
    
	 ?object(2,
		 #{ 'Type' => 'Outlines',
		    'Count' => 0 }),

	 ?object(3,
		 #{ 'Type' => 'Pages',
		    'Kids' => {?objref(4)},
		    'Count' => 1
		  }),

	 ?object(4,
		 #{ 'Type' => 'Page',
		    'Parent' => ?objref(3),
		    'MediaBox' => {0,0,612,792},
		    'Contents' => ?objref(5),
		    'Resources' => #{ 'ProcSet' => ?objref(6)}
		  }),

	 ?object(5,
		 ?stream(#{ 'Length' => 35 },
			 <<"  ... Page-marking operators ...  ">>)),

	 ?object(6,
		 {'PDF'})
	],
    print_obj_list(PDF).

test_lze_base85_0() ->
    Stream =
<<"J..)6T`?p&<!J9%_[umg\"B7/Z7KNXbN'S+,*Q/&\"OLT'FLIDK#!n`$\"<Atdi`\\Vn%b%)&'cA*VnK\\CJY(sF>c!Jnl@RM]WM;jjH6Gnc75idkL5]+cPZKEBPWdR>FF(kj1_R%W_d&/jS!;iuad7h?[L-F$+]]0A3Ck*$I0KZ?;<)CJtqi65XbVc3\\n5ua:Q/=0$W<#N3U;H,MQKqfg1?:lUpR;6oN[C2E4ZNr8Udn.'p+?#X+1>0Kuk$bCDF/(3fL5]Oq)^kJZ!C2H1'TO]Rl?Q:&'<5&iP!$Rq;BXRecDN[IJB`,)o8XJOSJ9sDS]hQ;Rj@!ND)bD_q&C\\g:inYC%)&u#:u,M6Bm%IY!Kb1+\":aAa'S`ViJglLb8<W9k6Yl\\0McJQkDeLWdPN?9A'jX*al>iG1p&i;eVoK&juJHs9%;Xomop\"5KatWRT\"JQ#qYuL,JD?M$0QP)lKn06l1apKDC@\\qJ4B!!(5m+j.7F790m(Vj88l8Q:_CZ(Gm1%X\\N1&u!FKHMB~>">>,
    Compressed = pdf:filter_ASCII85Decode(Stream),
    pdf:filter_LZWDecode(Compressed).


%% Test compression
test_lze_base85() ->
    List = [
     ?object(1, ?stream(#{ 'Length' => 534,
			   'Filter' => { 'ASCII85Decode', 'LZWDecode' }
			 },
			<<"J..)6T`?p&<!J9%_[umg\"B7/Z7KNXbN'S+,*Q/&\"OLT'F"
			  "LIDK#!n`$\"<Atdi`\\Vn%b%)&'cA*VnK\\CJY(sF>c!Jnl@"
			  "RM]WM;jjH6Gnc75idkL5]+cPZKEBPWdR>FF(kj1_R%W_d"
			  "&/jS!;iuad7h?[L-F$+]]0A3Ck*$I0KZ?;<)CJtqi65Xb"
			  "Vc3\\n5ua:Q/=0$W<#N3U;H,MQKqfg1?:lUpR;6oN[C2E4"
			  "ZNr8Udn.'p+?#X+1>0Kuk$bCDF/(3fL5]Oq)^kJZ!C2H1"
			  "'TO]Rl?Q:&'<5&iP!$Rq;BXRecDN[IJB`,)o8XJOSJ9sD"
			  "S]hQ;Rj@!ND)bD_q&C\\g:inYC%)&u#:u,M6Bm%IY!Kb1+"
			  "\":aAa'S`ViJglLb8<W9k6Yl\\0McJQkDeLWdPN?9A'jX*"
			  "al>iG1p&i;eVoK&juJHs9%;Xomop\"5KatWRT\"JQ#qYuL,"
			  "JD?M$0QP)lKn06l1apKDC@\\qJ4B!!(5m+j.7F790m(Vj8"
			  "8l8Q:_CZ(Gm1%X\\N1&u!FKHMB~>">>
		       )),
     %% same as 1?
     ?object(2, ?stream(#{ 'Length' => 568 },
			<<"2 J"
			  "BT\n"
			  "/F1 12 Tf\n"
			  "0 Tc\n"
			  "0 Tw\n"
			  "72.5 712 TD\n"
			  "[ ( Unfiltered streams can be read easily ) 65 ( , ) ] TJ\n"
			  "0 -14 TD\n"
			  "[ ( b ) 20 ( ut generally tak ) 10 ( e more space than \311 ) ] TJ\n"
			  "T* ( compressed streams . ) Tj\n"
			  "0 -28 TD\n"
			  "[ ( Se ) 25 ( v ) 15 ( eral encoding methods are a ) 20 ( v) 25 ( ailable in PDF ) 80 ( . ) ] TJ\n"
			  "0 -14 TD\n"
			  "( Some are used for compression and others simply ) Tj\n"
			  "T* [ ( to represent binary data in an ) 55 ( ASCII format . ) ] TJ\n"
			  "T* ( Some of the compression filters are \\n"
			  "suitable ) Tj\n"
			  "T* ( for both data and images , while others are \\n"
			  "suitable only ) Tj\n"
			  "T* ( for continuous-tone images . ) Tj\n"
			  "ET\n">>
			    ))
	   ],
    ?object(1, ?stream(Dict,Data)) = hd(List),
    { 'ASCII85Decode', 'LZWDecode' } = maps:get('Filter', Dict),
    Compressed = pdf:filter_ASCII85Decode(Data),
    pdf:filter_LZWDecode(Compressed).
    

%% H.3 Hello world example 

test_hello_world() ->
    PDF =
	[
	 ?object(1,
		 #{ 'Type' => 'Catalog',
		    'Outlines' => ?objref(2),
		    'Pages' => ?objref(3) }),
    
	 ?object(2,
		 #{ 'Type' => 'Outlines',
		    'Count' => 0 }),

	 ?object(3,
		 #{ 'Type' => 'Pages',
		    'Kids' => {?objref(4)},
		    'Count' => 1
		  }),

	 ?object(4,
		 #{ 'Type' => 'Page',
		    'Parent' => ?objref(3),
		    'MediaBox' => {0,0,612,792},
		    'Contents' => ?objref(5),
		    'Resources' => #{ 'ProcSet' => ?objref(6),
				      'Font' => #{ 'F1' => ?objref(7) }}
		  }),

	 ?object(5,
		 ?stream(#{ },
			 {text,
			  [{"Tf",['F1',24]},
			   {"Td",[100,100]},
			   {"Tj",["Hello World"]}
			  ]})),
	 ?object(6,
		 {'PDF','Text'}),
	 ?object(7,
		 #{ 'Type' => 'Font',
		    'SubType' => 'Type1',
		    'Name' => 'F1',
		    'BaseFont' => 'Helvetica',
		    'Encoding' => 'MacRomanEncoding'
		    })
		    
	],
    pdf:save("hello.pdf", PDF).

erl_forms_object(N, Module) ->
    Forms = funny:extract_forms(Module),
    Binary = term_to_binary(Forms),
    Compressed = pdf:filter_FlateEncode(Binary),
    Encoded = pdf:filter_ASCII85Encode(Compressed),
    ?object(N, ?stream(#{ 'Type' => 'ErlForms',
			  'Filter' => {'ASCII85Decode', 'FlateDecode'},
			  'Length' => byte_size(Encoded)
			}, Encoded)).

erl_beam_object(N, Module) ->
    {pdf,Beam,File} = code:get_object_code(Module),
    Compressed = pdf:filter_FlateEncode(Beam),
    Encoded = pdf:filter_ASCII85Encode(Compressed),
    ?object(N, ?stream(#{ 'Type' => 'Beam',
			  'FileName' => File,
			  'Filter' => {'ASCII85Decode', 'FlateDecode'},
			  'Length' => byte_size(Encoded)
			}, Encoded)).


test_erlforms() ->
    PDF =
	[
	 ?object(1,
		 #{ 'Type' => 'Catalog',
		    'Outlines' => ?objref(2),
		    'Pages' => ?objref(3) }),
    
	 ?object(2,
		 #{ 'Type' => 'Outlines',
		    'Count' => 0 }),

	 ?object(3,
		 #{ 'Type' => 'Pages',
		    'Kids' => {?objref(4)},
		    'Count' => 1
		  }),

	 ?object(4,
		 #{ 'Type' => 'Page',
		    'Parent' => ?objref(3),
		    'MediaBox' => {0,0,612,792},
		    'Contents' => ?objref(5),
		    'Resources' => #{ 'ProcSet' => ?objref(6),
				      'Font' => #{ 'F1' => ?objref(7) }}
		  }),

	 ?object(5,
		 ?stream(#{ },
			 {text,
			  [{"Tf",['F1',24]},
			   {"Td",[100,100]},
			   {"Tj",["Erlang forms"]}
			  ]})),
	 ?object(6,
		 {'PDF','Text'}),
	 ?object(7,
		 #{ 'Type' => 'Font',
		    'SubType' => 'Type1',
		    'Name' => 'F1',
		    'BaseFont' => 'Helvetica',
		    'Encoding' => 'MacRomanEncoding'
		    }),
	 erl_forms_object(8, pdf),
	 erl_beam_object(9, pdf)

	],
    pdf:save("erlforms.pdf", PDF).


%% H.4 SImple graphics example

test_graphics() ->
    PDF =
	[
	 ?object(1,
		 #{ 'Type' => 'Catalog',
		    'Outlines' => ?objref(2),
		    'Pages' => ?objref(3) }),
    
	 ?object(2,
		 #{ 'Type' => 'Outlines',
		    'Count' => 0 }),

	 ?object(3,
		 #{ 'Type' => 'Pages',
		    'Kids' => {?objref(4)},
		    'Count' => 1
		  }),

	 ?object(4,
		 #{ 'Type' => 'Page',
		    'Parent' => ?objref(3),
		    'MediaBox' => {0,0,612,792},
		    'Contents' => ?objref(5),
		    'Resources' => #{ 'ProcSet' => ?objref(6) }
		  }),
	 ?object(5,
		 ?stream(#{ },
			 {graphics,
			  [{"m",[150,250]},
			   {"l",[150,350]},
			   "S",
			   {"w",[4]},
			   {"d",[{4,6},0]},
			   {"m",[150,250]},
			   {"l",[400,250]},
			   "S",
			   {"d",[{},0]},
			   {"w",[1]},
			   {"RG",[1.0,0.0,0.0]},
			   {"rg",[0.5,0.75,1.0]},
			   {"re",[200,300,50,75]},
			   "B",
			   {"RG",[0.5,0.1,0.3]},
			   {"g", [0.7]},
			   {"m", [300,300]},
			   {"c", [300,400,400,400,400,300]},
			   "b"]})),
	 ?object(6,
		 {'PDF'})
		    
	],
    pdf:save("simple.pdf", PDF).

%% One page document
test_text_doc(Text) ->
    [
     ?object(1,
	     #{ 'Type' => 'Catalog',
		'Outlines' => ?objref(2),
		'Pages' => ?objref(3) }),
     ?object(2,
	     #{ 'Type' => 'Outlines',
		'Count' => 0 }),
     ?object(3,
	     #{ 'Type' => 'Pages',
		'Kids' => {?objref(4)},
		'Count' => 1
	      }),
     ?object(4,
	     #{ 'Type' => 'Page',
		'Parent' => ?objref(3),
		'MediaBox' => {0,0,612,792},
		'Contents' => ?objref(5),
		'Resources' => #{ 'ProcSet' => ?objref(6),
				  'Font' => #{ 'F1' => ?objref(7) }}
	      }),
     ?object(5,
	     ?stream(#{ }, {text, Text})),
     ?object(6,
	     {'PDF','Text'}),
     ?object(7,
	     #{ 'Type' => 'Font',
		'SubType' => 'Type1',
		'Name' => 'F1',
		'BaseFont' => 'Helvetica',
		'Encoding' => 'MacRomanEncoding'
	      })
    ].


print_obj_list([Obj|ObjList]) ->
    print_obj(Obj),
    print_obj_list(ObjList);
print_obj_list([]) ->
    ok.

print_obj(Obj) ->
    Data = pdf:format_obj(Obj),
    Length = iolist_size(Data),
    io:put_chars([integer_to_list(Length),":\n",Data,"\n"]),
    ok.

test_filter() ->
    StringList = 
	["", "0", "1", "01", "012", "0123", "01234", "012345",
	 [0], [0,0], [0,0,0], [0,0,0,0], [0,0,0,0,0], [0,0,0,0,0,0],
	 "Hello, World"],
    lists:foreach(
      fun(String) ->
	      io:format("test ASCIIHex ~p\n", [String]),
	      test_filter_ASCIIHex(String)
      end, StringList),

    lists:foreach(
      fun(String) ->
	      io:format("test ASCII85 ~p\n", [String]),
	      test_filter_ASCII85(String)
      end, StringList),
    ok.

test_filter_ASCIIHex(String) ->
    E1 = pdf:filter_ASCIIHexEncode(String),
    %% fixme: inject blank in E1!
    D1 = pdf:filter_ASCIIHexDecode(E1),
    D1 = iolist_to_binary(String),
    ok.

test_filter_ASCII85(String) ->
    E1 = pdf:filter_ASCII85Encode(String),
    %% fixme: inject blank in E1!
    D1 = pdf:filter_ASCII85Decode(E1),
    D1 = iolist_to_binary(String),
    ok.

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) ->
    fib(N, 0, 1).

fib(0, A, _B) -> A;
fib(1, _A, B) -> B;
fib(N, A, B) ->
    fib(N-1, B, A+B).

%% render erlang forms
test_erl_forms() ->
    Forms = function_forms(?MODULE, test_erl_forms, 0),
    String = lists:flatten(erl_prettypr:format(Forms)),
    Lines = string:tokens(String,"\n"),
    Text = [{"Tf",['F1',24]},
	    %% {"Tr",[1]},
	    {"TL",[25]},
	    {"Td",[10,750]}] ++
	lists:flatten([[{"rg",rand_color()},
			{"Tj",[Line]},'T*'] || Line <- Lines]),
    PDF = test_text_doc(Text),
    pdf:save("erl_forms.pdf", PDF).

rand_color() ->
    [(rand:uniform(256)-1)/255,
     (rand:uniform(256)-1)/255,
     (rand:uniform(256)-1)/255].

function_forms(M, F, A) ->
    Forms = extract_forms(M),
    case find_function(Forms, F, A) of
	false -> {error,not_found};
	FuncCode -> FuncCode
    end.

extract_forms(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
	non_existing -> {error,non_existing};
	Beam -> extract_forms(Beam)
    end;    
extract_forms(Beam) ->
    {ok,{_,[{_,Code}]}} = beam_lib:chunks(Beam, [abstract_code]),
    {raw_abstract_v1, Forms} = Code,
    Forms.

find_function(Forms, F, A) ->
    find(fun({function,_L,F0,A0,_}) ->
		 (F0 =:= F) andalso (A0 =:= A)
	 end, Forms).

%% find a matching element
find(Fun,[H|T]) ->
    try Fun(H) of
	true -> H;
	_ -> find(Fun,T)
    catch
	error:_ -> 
	    find(Fun,T)
    end;
find(_Fun,[]) ->
    false.

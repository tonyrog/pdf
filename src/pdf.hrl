-ifndef(__PDF_HRL__).
-define(__PDF_HRL__, true).

-define(is_byte(X), (((X) band (bnot 16#ff)) =:= 0)).

%% keywords
-define(null,       [null]).
-define(obj,        [obj]).
-define(endobj,     [endobj]).
-define(stream,     [stream]).
-define(endstream,  [endstream]).
-define(R,          ['R']).
-define(key(K),     [key|(K)]).

-define(objref(N), ['R',(N),0]).
-define(objref(N,G), ['R',(N),(G)]).

-define(freeref(N), ['F',(N),0]).
-define(freeref(N,G), ['F',(N),(G)]).

-define(object(N,Obj), [obj,(N),0,Obj]).
-define(object(N,G,Obj), [obj,(N),(G),Obj]).
-define(stream(Dict,Data), [stream,(Dict),(Data)]).

-define(PDF_VERSION, {1,4}).


-endif.

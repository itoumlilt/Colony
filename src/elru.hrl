-define(LOG(Msg,Arg), io:format(Msg ++ "~n",Arg)).
-define(CHILD(I, Type, Ar), {I, {I, start_link, Ar}, permanent, 5000, Type, [I]}).
%% Records
-record(mc_frame, {ip, obj, fn, dir, file, line}).

%% Memcheck: errors
-define(MC_ERR_UNV, 'UninitValue').
-define(MC_ERR_UNC, 'UninitCondition').
-define(MC_ERR_IR,  'InvalidRead').
-define(MC_ERR_IW,  'InvalidWrite').

%% Memcheck: leaks
-define(MC_LEAK_DEFINITIVELY, 'Leak_DefinitelyLost').
-define(MC_LEAK_POSSIBLY,     'Leak_PossiblyLost').

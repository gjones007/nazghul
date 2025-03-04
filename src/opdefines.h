_OP_DEF(opexe_0, "load", 1, 1, TST_STRING, OP_LOAD)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_T0LVL)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_T1LVL)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_READ_INTERNAL)
    _OP_DEF(opexe_0, "gensym", 0, 0, 0, OP_GENSYM)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_VALUEPRINT)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_EVAL)
#if USE_TRACING
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_REAL_EVAL)
#endif
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_E0ARGS)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_E1ARGS)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_APPLY)
#if USE_TRACING
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_REAL_APPLY)
    _OP_DEF(opexe_0, "tracing", 1, 1, TST_NATURAL, OP_TRACING)
#endif
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_DOMACRO)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LAMBDA)
    _OP_DEF(opexe_0, "make-closure", 1, 2, TST_PAIR TST_ENVIRONMENT, OP_MKCLOSURE)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_QUOTE)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_DEF0)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_DEF1)
    _OP_DEF(opexe_0, "defined?", 1, 2, TST_SYMBOL TST_ENVIRONMENT, OP_DEFP)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_BEGIN)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_IF0)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_IF1)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_SET0)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_SET1)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LET0)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LET1)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LET2)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LET0AST)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LET1AST)
    _OP_DEF(opexe_0, 0, 0, 0, 0, OP_LET2AST)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_LET0REC)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_LET1REC)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_LET2REC)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_COND0)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_COND1)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_DELAY)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_AND0)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_AND1)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_OR0)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_OR1)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_C0STREAM)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_C1STREAM)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_MACRO0)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_MACRO1)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_CASE0)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_CASE1)
    _OP_DEF(opexe_1, 0, 0, 0, 0, OP_CASE2)
    _OP_DEF(opexe_1, "eval", 1, 2, TST_ANY TST_ENVIRONMENT, OP_PEVAL)
    _OP_DEF(opexe_1, "apply", 1, INF_ARG, TST_NONE, OP_PAPPLY)
    _OP_DEF(opexe_1, "call-with-current-continuation", 1, 1, TST_NONE,
	OP_CONTINUATION)
#if USE_MATH
    _OP_DEF(opexe_2, "inexact->exact", 1, 1, TST_NUMBER, OP_INEX2EX)
    _OP_DEF(opexe_2, "exp", 1, 1, TST_NUMBER, OP_EXP)
    _OP_DEF(opexe_2, "log", 1, 1, TST_NUMBER, OP_LOG)
    _OP_DEF(opexe_2, "sin", 1, 1, TST_NUMBER, OP_SIN)
    _OP_DEF(opexe_2, "cos", 1, 1, TST_NUMBER, OP_COS)
    _OP_DEF(opexe_2, "tan", 1, 1, TST_NUMBER, OP_TAN)
    _OP_DEF(opexe_2, "asin", 1, 1, TST_NUMBER, OP_ASIN)
    _OP_DEF(opexe_2, "acos", 1, 1, TST_NUMBER, OP_ACOS)
    _OP_DEF(opexe_2, "atan", 1, 2, TST_NUMBER, OP_ATAN)
    _OP_DEF(opexe_2, "sqrt", 1, 1, TST_NUMBER, OP_SQRT)
    _OP_DEF(opexe_2, "expt", 2, 2, TST_NUMBER, OP_EXPT)
    _OP_DEF(opexe_2, "floor", 1, 1, TST_NUMBER, OP_FLOOR)
    _OP_DEF(opexe_2, "ceiling", 1, 1, TST_NUMBER, OP_CEILING)
    _OP_DEF(opexe_2, "truncate", 1, 1, TST_NUMBER, OP_TRUNCATE)
    _OP_DEF(opexe_2, "round", 1, 1, TST_NUMBER, OP_ROUND)
#endif
    _OP_DEF(opexe_2, "+", 0, INF_ARG, TST_NUMBER, OP_ADD)
    _OP_DEF(opexe_2, "-", 1, INF_ARG, TST_NUMBER, OP_SUB)
    _OP_DEF(opexe_2, "*", 0, INF_ARG, TST_NUMBER, OP_MUL)
    _OP_DEF(opexe_2, "/", 1, INF_ARG, TST_NUMBER, OP_DIV)
    _OP_DEF(opexe_2, "quotient", 1, INF_ARG, TST_INTEGER, OP_INTDIV)
    _OP_DEF(opexe_2, "remainder", 2, 2, TST_INTEGER, OP_REM)
    _OP_DEF(opexe_2, "modulo", 2, 2, TST_INTEGER, OP_MOD)
    _OP_DEF(opexe_2, "car", 1, 1, TST_PAIR, OP_CAR)
    _OP_DEF(opexe_2, "cdr", 1, 1, TST_PAIR, OP_CDR)
    _OP_DEF(opexe_2, "cons", 2, 2, TST_NONE, OP_CONS)
    _OP_DEF(opexe_2, "set-car!", 2, 2, TST_PAIR TST_ANY, OP_SETCAR)
    _OP_DEF(opexe_2, "set-cdr!", 2, 2, TST_PAIR TST_ANY, OP_SETCDR)
    _OP_DEF(opexe_2, "char->integer", 1, 1, TST_CHAR, OP_CHAR2INT)
    _OP_DEF(opexe_2, "integer->char", 1, 1, TST_NATURAL, OP_INT2CHAR)
    _OP_DEF(opexe_2, "char-upcase", 1, 1, TST_CHAR, OP_CHARUPCASE)
    _OP_DEF(opexe_2, "char-downcase", 1, 1, TST_CHAR, OP_CHARDNCASE)
    _OP_DEF(opexe_2, "symbol->string", 1, 1, TST_SYMBOL, OP_SYM2STR)
    _OP_DEF(opexe_2, "atom->string", 1, 1, TST_ANY, OP_ATOM2STR)
    _OP_DEF(opexe_2, "string->symbol", 1, 1, TST_STRING, OP_STR2SYM)
    _OP_DEF(opexe_2, "string->atom", 1, 1, TST_STRING, OP_STR2ATOM)
    _OP_DEF(opexe_2, "make-string", 1, 2, TST_NATURAL TST_CHAR, OP_MKSTRING)
    _OP_DEF(opexe_2, "string-length", 1, 1, TST_STRING, OP_STRLEN)
    _OP_DEF(opexe_2, "string-ref", 2, 2, TST_STRING TST_NATURAL, OP_STRREF)
    _OP_DEF(opexe_2, "string-set!", 3, 3, TST_STRING TST_NATURAL TST_CHAR,
	OP_STRSET)
    _OP_DEF(opexe_2, "string-append", 0, INF_ARG, TST_STRING, OP_STRAPPEND)
    _OP_DEF(opexe_2, "substring", 2, 3, TST_STRING TST_NATURAL, OP_SUBSTR)
    _OP_DEF(opexe_2, "vector", 0, INF_ARG, TST_NONE, OP_VECTOR)
    _OP_DEF(opexe_2, "make-vector", 1, 2, TST_NATURAL TST_ANY, OP_MKVECTOR)
    _OP_DEF(opexe_2, "vector-length", 1, 1, TST_VECTOR, OP_VECLEN)
    _OP_DEF(opexe_2, "vector-ref", 2, 2, TST_VECTOR TST_NATURAL, OP_VECREF)
    _OP_DEF(opexe_2, "vector-set!", 3, 3, TST_VECTOR TST_NATURAL TST_ANY, OP_VECSET)
    _OP_DEF(opexe_3, "not", 1, 1, TST_NONE, OP_NOT)
    _OP_DEF(opexe_3, "boolean?", 1, 1, TST_NONE, OP_BOOLP)
    _OP_DEF(opexe_3, "eof-object?", 1, 1, TST_NONE, OP_EOFOBJP)
    _OP_DEF(opexe_3, "null?", 1, 1, TST_NONE, OP_NULLP)
    _OP_DEF(opexe_3, "=", 2, INF_ARG, TST_NUMBER, OP_NUMEQ)
    _OP_DEF(opexe_3, "<", 2, INF_ARG, TST_NUMBER, OP_LESS)
    _OP_DEF(opexe_3, ">", 2, INF_ARG, TST_NUMBER, OP_GRE)
    _OP_DEF(opexe_3, "<=", 2, INF_ARG, TST_NUMBER, OP_LEQ)
    _OP_DEF(opexe_3, ">=", 2, INF_ARG, TST_NUMBER, OP_GEQ)
    _OP_DEF(opexe_3, "symbol?", 1, 1, TST_ANY, OP_SYMBOLP)
    _OP_DEF(opexe_3, "number?", 1, 1, TST_ANY, OP_NUMBERP)
    _OP_DEF(opexe_3, "string?", 1, 1, TST_ANY, OP_STRINGP)
    _OP_DEF(opexe_3, "integer?", 1, 1, TST_ANY, OP_INTEGERP)
    _OP_DEF(opexe_3, "real?", 1, 1, TST_ANY, OP_REALP)
    _OP_DEF(opexe_3, "char?", 1, 1, TST_ANY, OP_CHARP)
#if USE_CHAR_CLASSIFIERS
    _OP_DEF(opexe_3, "char-alphabetic?", 1, 1, TST_CHAR, OP_CHARAP)
    _OP_DEF(opexe_3, "char-numeric?", 1, 1, TST_CHAR, OP_CHARNP)
    _OP_DEF(opexe_3, "char-whitespace?", 1, 1, TST_CHAR, OP_CHARWP)
    _OP_DEF(opexe_3, "char-upper-case?", 1, 1, TST_CHAR, OP_CHARUP)
    _OP_DEF(opexe_3, "char-lower-case?", 1, 1, TST_CHAR, OP_CHARLP)
#endif
    _OP_DEF(opexe_3, "port?", 1, 1, TST_ANY, OP_PORTP)
    _OP_DEF(opexe_3, "input-port?", 1, 1, TST_ANY, OP_INPORTP)
    _OP_DEF(opexe_3, "output-port?", 1, 1, TST_ANY, OP_OUTPORTP)
    _OP_DEF(opexe_3, "procedure?", 1, 1, TST_ANY, OP_PROCP)
    _OP_DEF(opexe_3, "pair?", 1, 1, TST_ANY, OP_PAIRP)
    _OP_DEF(opexe_3, "list?", 1, 1, TST_ANY, OP_LISTP)
    _OP_DEF(opexe_3, "environment?", 1, 1, TST_ANY, OP_ENVP)
    _OP_DEF(opexe_3, "vector?", 1, 1, TST_ANY, OP_VECTORP)
    _OP_DEF(opexe_3, "eq?", 2, 2, TST_ANY, OP_EQ)
    _OP_DEF(opexe_3, "eqv?", 2, 2, TST_ANY, OP_EQV)
    _OP_DEF(opexe_4, "force", 1, 1, TST_ANY, OP_FORCE)
    _OP_DEF(opexe_4, 0, 0, 0, 0, OP_SAVE_FORCED)
    _OP_DEF(opexe_4, "write", 1, 2, TST_ANY TST_OUTPORT, OP_WRITE)
    _OP_DEF(opexe_4, "write-char", 1, 2, TST_CHAR TST_OUTPORT, OP_WRITE_CHAR)
    _OP_DEF(opexe_4, "display", 1, 2, TST_ANY TST_OUTPORT, OP_DISPLAY)
    _OP_DEF(opexe_4, "newline", 0, 1, TST_OUTPORT, OP_NEWLINE)
    _OP_DEF(opexe_4, "error", 1, INF_ARG, TST_NONE, OP_ERR0)
    _OP_DEF(opexe_4, 0, 0, 0, 0, OP_ERR1)
    _OP_DEF(opexe_4, "reverse", 1, 1, TST_PAIR, OP_REVERSE)
    _OP_DEF(opexe_4, "list*", 1, INF_ARG, TST_NONE, OP_LIST_STAR)
    _OP_DEF(opexe_4, "append", 0, INF_ARG, TST_NONE, OP_APPEND)
    _OP_DEF(opexe_4, "put", 3, 3, TST_NONE, OP_PUT)
    _OP_DEF(opexe_4, "get", 2, 2, TST_NONE, OP_GET)
    _OP_DEF(opexe_4, "quit", 0, 1, TST_NUMBER, OP_QUIT)
    _OP_DEF(opexe_4, "gc", 0, 0, 0, OP_GC)
    _OP_DEF(opexe_4, "gc-verbose", 0, 1, TST_NONE, OP_GCVERB)
    _OP_DEF(opexe_4, "new-segment", 0, 1, TST_NUMBER, OP_NEWSEGMENT)
    _OP_DEF(opexe_4, "oblist", 0, 0, 0, OP_OBLIST)
    _OP_DEF(opexe_4, "current-input-port", 0, 0, 0, OP_CURR_INPORT)
    _OP_DEF(opexe_4, "current-output-port", 0, 0, 0, OP_CURR_OUTPORT)
    _OP_DEF(opexe_4, "open-input-file", 1, 1, TST_STRING, OP_OPEN_INFILE)
    _OP_DEF(opexe_4, "open-output-file", 1, 1, TST_STRING, OP_OPEN_OUTFILE)
    _OP_DEF(opexe_4, "open-input-output-file", 1, 1, TST_STRING, OP_OPEN_INOUTFILE)
#if USE_STRING_PORTS
    _OP_DEF(opexe_4, "open-input-string", 1, 1, TST_STRING, OP_OPEN_INSTRING)
    _OP_DEF(opexe_4, "open-output-string", 1, 1, TST_STRING, OP_OPEN_OUTSTRING)
    _OP_DEF(opexe_4, "open-input-output-string", 1, 1, TST_STRING,
	OP_OPEN_INOUTSTRING)
#endif
    _OP_DEF(opexe_4, "close-input-port", 1, 1, TST_INPORT, OP_CLOSE_INPORT)
    _OP_DEF(opexe_4, "close-output-port", 1, 1, TST_OUTPORT, OP_CLOSE_OUTPORT)
    _OP_DEF(opexe_4, "interaction-environment", 0, 0, 0, OP_INT_ENV)
    _OP_DEF(opexe_4, "current-environment", 0, 0, 0, OP_CURR_ENV)
    _OP_DEF(opexe_5, "read", 0, 1, TST_INPORT, OP_READ)
    _OP_DEF(opexe_5, "read-char", 0, 1, TST_INPORT, OP_READ_CHAR)
    _OP_DEF(opexe_5, "peek-char", 0, 1, TST_INPORT, OP_PEEK_CHAR)
    _OP_DEF(opexe_5, "char-ready?", 0, 1, TST_INPORT, OP_CHAR_READY)
    _OP_DEF(opexe_5, "set-input-port", 1, 1, TST_INPORT, OP_SET_INPORT)
    _OP_DEF(opexe_5, "set-output-port", 1, 1, TST_OUTPORT, OP_SET_OUTPORT)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDSEXPR)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDLIST)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDDOT)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDQUOTE)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDQQUOTE)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDQQUOTEVEC)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDUNQUOTE)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDUQTSP)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_RDVEC)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_P0LIST)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_P1LIST)
    _OP_DEF(opexe_5, 0, 0, 0, 0, OP_PVECFROM)
    _OP_DEF(opexe_6, "length", 1, 1, TST_LIST, OP_LIST_LENGTH)
    _OP_DEF(opexe_6, "assq", 2, 2, TST_NONE, OP_ASSQ)
    _OP_DEF(opexe_6, "get-closure-code", 1, 1, TST_NONE, OP_GET_CLOSURE)
    _OP_DEF(opexe_6, "closure?", 1, 1, TST_NONE, OP_CLOSUREP)
    _OP_DEF(opexe_6, "macro?", 1, 1, TST_NONE, OP_MACROP)
#if USE_REENTER
    _OP_DEF(opexe_ghul, 0, 0, 0, 0, OP_EXIT_REENTER)
#endif
#undef _OP_DEF

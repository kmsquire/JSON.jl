module JSON

export json # returns a compact JSON representation as a String

include("Parser.jl")

import .Parser.parse

type State
    io::IO
    indentstep::Int
    doindent::Bool
    indentlen::Int
    prefix::String
    sufix::String
    otype::Array{Bool, 1}
    State(io::IO, indentstep::Int) = new(io, 
                                         indentstep, 
                                         indentstep > 0,
                                         0, 
                                         "", 
                                         indentstep > 0 ? "\n" : "",
                                         Bool[])
end

function set_state(state::State, operate::Int)
    state.indentlen += state.indentstep * operate
    state.prefix = " "^state.indentlen
end

function start_object(state::State, is_dict::Bool)
    if state.doindent
        was_dict = length(state.otype) == 0 ? false : last(state.otype)
        if was_dict
            Base.print(state.io, state.sufix, state.prefix)
        end
        push!(state.otype, is_dict)
        Base.print(state.io, is_dict ? "{": "[", state.sufix)
        set_state(state, 1)
    else
        Base.print(state.io, is_dict ? "{": "[")
    end
end

function end_object(state::State, is_dict::Bool)
    if state.doindent
        set_state(state, -1)
        pop!(state.otype)
        Base.print(state.io, state.sufix, state.prefix, is_dict ? "}": "]")
    else
        Base.print(state.io, is_dict ? "}": "]")
    end
end

function print_escaped(io::IO, s::String)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\\'       ? Base.print(io, "\\\\") :
        c == '"'        ? Base.print(io, "\\\"") :
        8 <= c <= 10    ? Base.print(io, '\\', "btn"[c-7]) :
        c == '\f'       ? Base.print(io, "\\f") :
        c == '\r'       ? Base.print(io, "\\r") :
        isprint(c)      ? Base.print(io, c) :
        c <= '\x7f'     ? Base.print(io, "\\u", hex(c, 4)) :
                          Base.print(io, c) #JSON is UTF8 encoded
        i = j
    end
end

function _print(state::State, s::String)
    Base.print(state.io, '"')
    JSON.print_escaped(state.io, s)
    Base.print(state.io, '"')
end

function _print(state::State, s::Union(Integer, FloatingPoint))
    if isnan(s) || isinf(s)
        Base.print(state.io, "null")
    else
        Base.print(state.io, s)
    end
end

function _print(state::State, n::Nothing)
    Base.print(state.io, "null")
end

function _print(state::State, a::Associative)
    start_object(state, true)
    first = true
    for (key, value) in a
        first ? (first = false) : Base.print(state.io, ",", state.sufix)
        Base.print(state.io, state.prefix)
        JSON._print(state, string(key))
        Base.print(state.io, state.doindent ? ": " : ":")
        JSON._print(state, value)
    end
    end_object(state, true)
end

function _print(state::State, a::Union(AbstractVector,Tuple))
    if length(a) > 0
        start_object(state, false)
        Base.print(state.io, state.prefix)
        for x in a[1:end-1]
            JSON._print(state, x)
            state.doindent ? Base.print(state.io, ",", state.sufix, state.prefix) : 
                             Base.print(state.io, ",")
        end

        try
            JSON._print(state, a[end])
        catch
            # Potentially we got here by accessing
            # something through a 0 dimensional
            # part of an array. Probably expected
            # behavior is to not print and move on
        end
        end_object(state, false)
    else
        Base.print(state.io, "[]")
    end
end

function _print(state::State, a)
    start_object(state, true)
    range = typeof(a).names
    if length(range) > 0
        Base.print(state.io, state.prefix, "\"", range[1], state.doindent ? "\": " : "\":")
        JSON._print(state, a.(range[1]))

        for name in range[2:end]
            state.doindent ? Base.print(state.io, ",", state.sufix, state.prefix, "\"", name, "\": ") :
                             Base.print(state.io, ",\"", name, "\":")
            JSON._print(state, a.(name))
        end
    end
    end_object(state, true)
end

function _print(state::State, f::Function)
    Base.print(state.io, "\"function at ", f.fptr, "\"")
end

function _print(state::State, d::DataType)
    Base.print(state.io, d)
end

# Note: Arrays are printed in COLUMN MAJOR format.
# i.e. json([1 2 3; 4 5 6]) == "[[1,4],[2,5],[3,6]]"
function _print{T, N}(state::State, a::AbstractArray{T, N})
    lengthN = size(a, N)
    if lengthN >= 0
        start_object(state, false)
        newdims = ntuple(N - 1, i -> 1:size(a, i))
        Base.print(state.io, state.prefix)
        JSON._print(state, slice(a, newdims..., 1))

        for j in 2:lengthN
            state.doindent ? Base.print(state.io, ",", state.sufix, state.prefix) :
                             Base.print(state.io, ",")

            newdims = ntuple(N - 1, i -> 1:size(a, i))
            JSON._print(state, slice(a, newdims..., j))
        end
        end_object(state, false)
    else
        Base.print(state.io, "[]")
    end
end

function print(io::IO, a, indent=0)
    JSON._print(State(io, indent), a)
    if indent > 0
        Base.print(io, "\n")
    end
end

function print(a, indent=0)
    JSON._print(State(STDOUT, indent), a)
    if indent > 0
        println()
    end
end

json(a, indent=0) = sprint(JSON.print, a, indent)

function determine_bracket_type(io::IO)
    open_bracket = close_bracket = nothing
    while open_bracket == nothing
        eof(io) && throw(EOFError())
        c = read(io, Char)
        if c == '{'
            open_bracket = '{'
            close_bracket = '}'
        elseif c == '['
            open_bracket = '['
            close_bracket = ']'
        elseif c == '\0'
            throw(EOFError())
        end
    end
    open_bracket, close_bracket
end

###
# Consume a string (even if it is invalid), with ack to Douglas Crockford.
# On entry we must already have consumed the opening quotation double-quotation mark
# Add the characters of the string to obj
function consumeString(io::IO, obj::IOBuffer)
    c = '"'

    # When parsing for string values, we must look for " and \ characters.
    while true
        eof(io) && throw(EOFError())
        c = read(io, Char)
        if c == '"'
            write(obj, c)
            return
        end
        if c == '\\'
            write(obj, c)
            eof(io) && throw(EOFError())
            c = read(io, Char)
        end
        write(obj, c)
    end
    throw(EOFError())
end

function parse(io::IO; ordered::Bool=false)
    open_bracket = close_bracket = nothing
    try
        open_bracket, close_bracket = determine_bracket_type(io)
    catch exception
        isa(exception, EOFError) && return
    end
    num_brackets_needed = 1

    obj = IOBuffer()
    write(obj, open_bracket)

    while num_brackets_needed > 0
        eof(io) && throw(EOFError())
        c = read(io, Char)
        write(obj, c)

        if c == open_bracket
            num_brackets_needed += 1
        elseif c == close_bracket
            num_brackets_needed -= 1
        elseif c == '"'
            consumeString(io, obj)
        end
    end
    JSON.parse(takebuf_string(obj), ordered=ordered)
end

function parsefile(filename::String; ordered::Bool=false, use_mmap=true)
    sz = filesize(filename)
    open(filename) do io
        s = use_mmap ? UTF8String(mmap_array(Uint8, (sz,), io)) : readall(io)
        JSON.parse(s, ordered=ordered)
    end
end

end # module


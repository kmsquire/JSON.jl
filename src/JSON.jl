module JSON

export json # returns a compact JSON representation as a String

include("Parser.jl")

import .Parser.parse

const INDENT=true
const NOINDENT=false

type State{I}
    indentstep::Int
    indentlen::Int
    prefix::String
    otype::Array{Bool, 1}

    State(indentstep::Int) = new(indentstep,
                                 0,
                                 "",
                                 Bool[])
end

State(indentstep::Int=0) = State{indentstep>0}(indentstep)

suffix(::State{INDENT}) = "\n"
suffix(::State{NOINDENT}) = ""

prefix(s::State{INDENT}) = s.prefix
prefix(::State{NOINDENT}) = ""

space(::State{INDENT}) = " "
space(::State{NOINDENT}) = ""

function set_state(state::State{INDENT}, operate::Int)
    state.indentlen += state.indentstep * operate
    state.prefix = " "^state.indentlen
end

function start_object(io::IO, state::State{INDENT}, is_dict::Bool)
    was_dict = length(state.otype) == 0 ? false : last(state.otype)
    if was_dict
        Base.print(io, suffix(state), prefix(state))
    end
    push!(state.otype, is_dict)
    Base.print(io, is_dict ? "{": "[", suffix(state))
    set_state(state, 1)
end

function end_object(io::IO, state::State{INDENT}, is_dict::Bool)
    set_state(state, -1)
    pop!(state.otype)
    Base.print(io, suffix(state), prefix(state), is_dict ? "}": "]")
end

function start_object(io::IO, state::State{NOINDENT}, is_dict::Bool)
    Base.print(io, is_dict ? "{": "[")
end

function end_object(io::IO, state::State{NOINDENT}, is_dict::Bool)
    Base.print(io, is_dict ? "}": "]")
end

const unescaped = Bool[isprint(c) && !(c in ['\\','"']) for c in '\x00':'\x7F']

function print_escaped(io, s::String)
    for c in s
        c <= '\x7f' ? (unescaped[c+1]  ? Base.print(io, c) :
                       c == '\\'       ? Base.print(io, "\\\\") :
                       c == '"'        ? Base.print(io, "\\\"") :
                       8 <= c <= 10    ? Base.print(io, '\\', "btn"[c-7]) :
                       c == '\f'       ? Base.print(io, "\\f") :
                       c == '\r'       ? Base.print(io, "\\r") :
                                         Base.print(io, "\\u", hex(c, 4))) :
                      Base.print(io, c) #JSON is UTF8 encoded
    end
end

function print(io::IO, state::State, s::String)
    Base.print(io, '"')
    JSON.print_escaped(io, s)
    Base.print(io, '"')
end

function print(io::IO, state::State, s::Union(Integer, FloatingPoint))
    if isnan(s) || isinf(s)
        Base.print(io, "null")
    else
        Base.print(io, s)
    end
end

function print(io::IO, state::State, n::Nothing)
    Base.print(io, "null")
end

function print(io::IO, state::State, a::Associative)
    start_object(io, state, true)
    first = true
    for (key, value) in a
        first ? (first = false) : Base.print(io, ",", suffix(state))
        Base.print(io, prefix(state))
        JSON.print(io, state, string(key))
        Base.print(io, ":", space(state))
        JSON.print(io, state, value)
    end
    end_object(io, state, true)
end

function print(io::IO, state::State, a::Union(AbstractVector,Tuple))
    if length(a) > 0
        start_object(io, state, false)
        Base.print(io, prefix(state))
        i = start(a)
        if !done(a,i)
            (x, i) = next(a, i)
            JSON.print(io, state, x)
        end
        while !done(a,i)
            (x, i) = next(a, i)
            Base.print(io, ",", suffix(state), prefix(state))
            JSON.print(io, state, x)
        end

        end_object(io, state, false)
    else
        Base.print(io, "[]")
    end
end

function print(io::IO, state::State, a)
    start_object(io, state, true)
    range = typeof(a).names
    if length(range) > 0
        Base.print(io, prefix(state), "\"", range[1], "\":", space(state))
        JSON.print(io, state, a.(range[1]))

        for name in range[2:end]
            Base.print(io, ",", suffix(state), prefix(state), "\"", name, "\":", space(state))
            JSON.print(io, state, a.(name))
        end
    end
    end_object(io, state, true)
end

function print(io::IO, state::State, f::Function)
    Base.print(io, "\"function at ", f.fptr, "\"")
end

function print(io::IO, state::State, d::DataType)
    Base.print(io, d)
end

# Note: Arrays are printed in COLUMN MAJOR format.
# i.e. json([1 2 3; 4 5 6]) == "[[1,4],[2,5],[3,6]]"
function print{T,N}(io::IO, state::State, a::AbstractArray{T,N})
    lengthN = size(a, N)
    if lengthN >= 0
        start_object(io, state, false)
        newdims = ntuple(N - 1, i -> 1:size(a, i))
        Base.print(io, prefix(state))
        JSON.print(io, state, slice(a, newdims..., 1))

        for j in 2:lengthN
            Base.print(io, ",", suffix(state), prefix(state))

            newdims = ntuple(N - 1, i -> 1:size(a, i))
            JSON.print(io, state, slice(a, newdims..., j))
        end
        end_object(io, state, false)
    else
        Base.print(io, "[]")
    end
end

function print(io::IO, a, indent=0)
    JSON.print(io, State(indent), a)
    if indent > 0
        Base.print(io, "\n")
    end
end

function print(a, indent=0)
    JSON.print(STDOUT, State(indent), a)
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


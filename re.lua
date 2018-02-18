-- $Id: re.lua,v 1.44 2013/03/26 20:11:40 roberto Exp $

-- imported functions and modules
local tonumber, type, print, error = tonumber, type, print, error
local setmetatable = setmetatable
local m = require"lpeg"
local version = _VERSION
if version == "Lua 5.2" then _ENV = nil end

local any = m.P(1)

-- pattern's metatable
local mt = getmetatable(any)

-- Pre-defined names
local Predef = { nl = m.P"\n", b = m.B(-1) }

local mem
local fmem
local gmem

local function updatelocale ()
  m.locale(Predef)
  Predef.a = Predef.alpha
  Predef.c = Predef.cntrl
  Predef.d = Predef.digit
  Predef.g = Predef.graph
  Predef.l = Predef.lower
  Predef.p = Predef.punct
  Predef.s = Predef.space
  Predef.u = Predef.upper
  Predef.w = Predef.alnum
  Predef.x = Predef.xdigit
  Predef.A = m.N(Predef.a)
  Predef.C = m.N(Predef.c)
  Predef.D = m.N(Predef.d)
  Predef.G = m.N(Predef.g)
  Predef.L = m.N(Predef.l)
  Predef.P = m.N(Predef.p)
  Predef.S = m.N(Predef.s)
  Predef.U = m.N(Predef.u)
  Predef.W = m.N(Predef.w)
  Predef.X = m.N(Predef.x)
  mem = {}    -- restart memoization
  fmem = {}
  gmem = {}
  local mt = {__mode = "v"}
  setmetatable(mem, mt)
  setmetatable(fmem, mt)
  setmetatable(gmem, mt)
end

updatelocale()

local function getdef (id, defs)
  local c = defs and defs[id]
  if not c then error("undefined name: " .. id) end
  return c
end

local function patt_error (s, i)
  local msg = (#s < i + 20) and s:sub(i)
                             or s:sub(i,i+20) .. "..."
  msg = ("pattern error near '%s'"):format(msg)
  error(msg, 2)
end

local function equalcap (s, i, c)
  if type(c) ~= "string" then return nil end
  local e = #c + i
  if s:sub(i, e - 1) == c then return e else return nil end
end

local S = (Predef.space + "--" * m.N(Predef.nl)^0)^0

local name = m.R("AZ", "az", "__") * m.R("AZ", "az", "__", "09")^0

local arrow = S * "<-"

local seq_follow = m.P"/" + ")" + "}" + ":}" + "~}" + "|}" + (name * arrow) + -1

name = m.C(name)

-- a defined name only have meaning in a given environment
local Def = name * m.Carg(1)

local num = m.C(m.R"09"^1) * S / tonumber
local sign_num = m.C(m.S"+-" * m.R"09"^1) / tonumber

local String = "'" * m.C(m.N"'"^0) * "'" +
               '"' * m.C(m.N'"'^0) * '"'

local defined = "%" * Def / function (c,Defs)
  local cat = Defs and Defs[c]
  if cat then return m.P(cat) end
  return Predef[c] or error ("name '" .. c .. "' undefined")
end

local Range = m.Cs(any * (m.P"-"/"") * m.N"]") / m.R

local item = defined + Range + m.C(any)

local Class =
    "["
  * (m.C(m.P"^"^-1))    -- optional complement symbol
  * m.Cf(item * (item - "]")^0, mt.__add) /
    function (c, p) return m.N(p, c == '^') end
  * "]"

local function adddef (t, k, exp)
  if t[k] then
    error("'"..k.."' already defined as a rule")
  else
    t[k] = exp
  end
  return t
end

local function firstdef (n, r) return adddef({n}, n, r) end

local function NT (n, b)
  if not b then
    error("rule '"..n.."' used outside a grammar")
  else return m.V(n)
  end
end

function m.M(pat, skip)         -- lpeg.M for anywhere search
  if not skip then skip = any * m.N(pat)^0 end
  return m.P{pat + skip * m.V(1)}
end

local function re_pow (p, n)
  if p == Predef.b then return m.B(-n) end
  return mt.__pow(p, n, true)   -- true pow = no rep code
end

local exp = m.P{ "Exp",
  Exp = S * ( m.V"Grammar"
            + m.Cf(m.V"Seq" * ("/" * S * m.V"Seq")^0, mt.__add) );
  Seq = m.Cf(m.Cc(m.P"") * m.V"Prefix"^0 , mt.__mul)
        * (#seq_follow + patt_error);
  Prefix = "&" * S * m.V"Prefix" / mt.__len -- look ahead
         + "@" * S * m.V"Prefix" / m.B      -- look behind
         + ">" * S * m.V"Prefix" / m.M      -- look forward
         + "<" * S * m.Cg(m.V"Prefix" * m.Cc(Predef.b)) / m.M
         + "!" * S * m.V"Prefix" / mt.__unm
         + m.V"Suffix";
  Suffix = m.Cf(m.V"Primary" * S *
          ( ( m.P"+" * m.Cc(1, mt.__pow)
            + m.P"*" * m.Cc(0, mt.__pow)
            + m.P"?" * m.Cc(-1, mt.__pow)
            + "^" * (m.Cg(num * m.Cc(re_pow)) + m.Cg(sign_num * m.Cc(mt.__pow)))
            + "->" * S * ( m.Cg((String + num + sign_num) * m.Cc(mt.__div))
                         + m.P"{}" * m.Cc(nil, m.Ct)
                         + m.Cg(Def / getdef * m.Cc(mt.__div))
                         )
            + "=>" * S * m.Cg(Def / getdef * m.Cc(m.Cmt))
            + "~>" * S * m.Cg(Def / getdef * m.Cc(m.Cf))    -- folding capture
            ) * S
          )^0, function (a,b,f) return f(a,b) end );
  Primary = "(" * m.V"Exp" * ")"
            + String / m.P
            + Class
            + defined
            + "{:" * (name * ":" + m.Cc(nil)) * m.V"Exp" * ":}" /
                     function (n, p) return m.Cg(p, n) end
            + "=" * name / function (n) return m.Cmt(m.Cb(n), equalcap) end
            + m.P"{}" / m.Cp
            + "{~" * m.V"Exp" * "~}" / m.Cs
            + "{|" * m.V"Exp" * "|}" / m.Ct
            + "{" * m.V"Exp" * "}" / m.C
            + m.P"." * m.Cc(any)
            + (name * -arrow + "<" * name * ">") * m.Cb("G") / NT;
  Definition = name * arrow * m.V"Exp";
  Grammar = m.Cg(m.Cc(true), "G") *
            m.Cf(m.V"Definition" / firstdef * m.Cg(m.V"Definition")^0,
              adddef) / m.P
}

local pattern = S * m.Cg(m.Cc(false), "G") * exp / m.P * (-any + patt_error)

local function compile (p, defs)
  if m.type(p) then return p end    -- already compiled
  local cp = m.match(pattern, p, 1, defs)
  if cp == nil then error("incorrect pattern", 3) end
  return cp
end

local function match (s, p, i, def) -- def -> no memoize
  if def then return m.match(compile(p, def), s, i or 1) end
  local cp = mem[p]
  if cp == nil then
    cp = compile(p)
    mem[p] = cp
  end
  return m.match(cp, s, i or 1)
end

local function find (s, p, i)
  local cp = fmem[p]
  if not cp then
    cp = compile(p) / 0     -- string.find behavior
    cp = m.M(m.Cp() * cp) * (Predef.b * m.Cp() + m.Cc(0))
    fmem[p] = cp
  end
  return m.match(cp, s, i or 1)
end

local function gsub (s, p, rep)
  local g = gmem[p] or {}   -- ensure gmem[p] is not collected while here
  gmem[p] = g
  local cp = g[rep]
  if not cp then
    cp = compile(p)
    cp = m.Cs((cp / rep + any * m.N(cp)^0)^0)
    g[rep] = cp
  end
  return m.match(cp, s)
end

-- exported names
local re = {
  compile = compile,
  match = match,
  find = find,
  gsub = gsub,
  pow = re_pow,
  def = Predef,
  updatelocale = updatelocale,
}

function re.split(sep, elem)
  if sep == '' then return m.C(1)^1 end -- split by chars if ''
  sep = compile(sep or '%s')            -- default sep = spaces
  if elem == '' then
    elem = m.C((1-sep)^0)               -- empty elem captured
    return elem * (sep * elem)^0
  else
    elem = m.C(any * (1-sep)^0)         -- non-empty elem only
    return sep^0 * elem * (sep^1 * elem)^0
  end
end

if version == "Lua 5.1" then _G.re = re end

return re

# LPeg-anywhere
match anywhere, even backwards (see **README** for details)

### Added 3 lpeg functions:
**lpeg.B(-n)** move back n bytes   
**lpeg.B(-patt)** move back fixedlen(patt)  

**lpeg.N(patt)** return a set of non-head-chars, useful for optimized search  
  
**lpeg.M(patt, skip)** (lua function in **re.lua**) that can match anywhere **FAST**  
   
**Example:**  return 2 captures ". -and(. *)" and ". *and(. *)"  
t = "this and that and this and more"  
  
-- Using **lpeg**:  
all = lpeg.C(lpeg.P(1)^0)  
pat = lpeg.M('and') * all * lpeg.M('and', lpeg.B(-1)) * all  
  
= lpeg.match(pat, t)  
 that and this and more       more  
  
-- Using **lpeg re**:  
-- For convenience, re.lua added '>' and '<' *prefix* for forward and backward match:  
  
= re.match(t, ">'and' {. *} <'and' {. *}")  
 that and this and more       more  
 

# LPeg-anywhere
match anywhere, even backwards (see **README** for details)

### Added 3 lpeg functions:
**lpeg.B(-n)** move back n bytes   
**lpeg.B(-patt)** move back fixedlen(patt)  

**lpeg.H(patt)** return a set of head-chars, useful for optimized search  
  
**lpeg.M(patt, skip)** (a lua function) that can match anywhere **FAST**  
-- **lpeg.M** now reside in **re.lua**, but I really like it stay in lpeg C source.  
-- if anyone can make **lpeg.M** in C, please let me know ...  
  
**Example:**  
  
t = "this and that and whatever"  
= **lpeg.M**('whatever'):match(t)  
27  -- match *forward* to end-of-string  
  
= lpeg.match(**lpeg.M**('that', lpeg.B(-1)), t, -1)  
14  -- match *backward* from last byte of t (the 'r')     



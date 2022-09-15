module Datatype where 

 data Datatype = Int | Op 
 datatype:: Char -> Maybe Datatype
 datatype c = case c of
                Int -> int(c);
                Op -> op(c);
                _ -> Nothing

 data Op = Char
 op:: Char -> IO()
 op c = print c

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>),(<*>))
import Data.List (elemIndex)
import Data.Char (toLower, toUpper,chr)
import Data.Maybe (fromJust)
data Symbol = NT String | T String
            deriving (Show,Eq,Ord,Read)

data Rule = Rule {name :: String, definition :: RHS}
            deriving (Show,Eq,Ord,Read)
data RHS = Alternation [RHS]
         | Sequence [RHS]
         | Epsilon
         | Charset String
         | Repeat (Int,Maybe Int) RHS
         | Symbol Symbol
           deriving (Show, Eq, Ord, Read)
infixl 2 =>>
(=>>) :: (Monad m) => m a -> m b -> m a
a =>> b = a >>= \x -> b >> return x

number_in_base :: Int -> Parser Int
number_in_base base = chainl1 dig (return $ \a n -> a * base + n) 
  where charset = take base "0123456789abcdef"
        dig :: Parser Int
        dig = fmap (fromJust . flip elemIndex charset . toLower) $ oneOf (charset ++ map toUpper charset)
          

postels_crlf :: Parser ()
postels_crlf = ((char '\n' >> return ()) <|> (char '\r' >> optional (char '\n')))
wsp = oneOf " \t"
vchar = oneOf ['\x21'..'\x7e']
dquote = char '"'

rulelist :: Parser [Rule]
rulelist = many1 rule

rule :: Parser Rule
rule = Rule <$> (rulename =>> defined_as)
            <*> (elements =>> many c_wsp)

rulename :: Parser String
rulename = (:) <$> letter <*> many (letter <|> digit <|> char '-')

defined_as :: Parser ()
defined_as = many c_wsp >> char '=' >> optional (char '/') >> (many c_wsp >> return ())

elements :: Parser RHS
elements = alternation =>> many c_wsp

c_wsp = (wsp >> return ()) <|> (c_nl >> optional wsp)

c_nl = comment <|> postels_crlf
comment = char ';' >> many (wsp <|> vchar) >> postels_crlf

alternation, concatenation, repetition :: Parser RHS
alternation = Alternation <$> sepBy1 concatenation (try $ many c_wsp >> char '/' >> many c_wsp) =>> skipMany c_wsp
concatenation =  Sequence <$> sepBy1 repetition (try $ (many1 c_wsp >> notFollowedBy (oneOf "/)>")))
repetition = do f <- choice [try $ do n1 <- option 0 (number_in_base 10)
                                      char '*'
                                      n2 <- option Nothing (Just <$> number_in_base 10)
                                      return (Repeat (n1, n2))
                            , do n <- number_in_base 10
                                 return (Repeat (n,Just n))
                            , return id]
                elem <- element
                return ((f :: RHS -> RHS) (elem :: RHS))

element :: Parser RHS
element = Symbol <$> T <$> rulename 
          <|> group 
          <|> abnf_option 
          <|> char_val 
          <|> num_val 
          <|> prose_val
group, abnf_option :: Parser RHS
group = between (char '(') (char ')') $ many c_wsp >> alternation =>> many c_wsp
abnf_option = fmap (\x -> Alternation [Epsilon,x]) $ between (char '[') (char ']') $ many c_wsp >> alternation =>> many c_wsp
char_val = Symbol <$> T <$> (between dquote dquote $ many (oneOf ("\x20\x21" ++ ['\x23'..'\x7e'])))
num_val = char '%' >> (bin_val <|> hex_val <|> dec_val)
bin_val = char 'b' >>  multi_range_in_base 2
hex_val = char 'x' >> multi_range_in_base 16
dec_val = char 'd' >> multi_range_in_base 10

multi_range_in_base :: Int -> Parser RHS
multi_range_in_base base = do
  start <- number_in_base base
  cs <- choice [(:) <$> return start <*> many1 (char '.' >> number_in_base base)
               ,char '-' >> number_in_base base >>= \end -> return [start..end]
               ,return [start]]
  return $ Charset (fmap chr cs)
prose_val :: Parser RHS
prose_val = between (char '<') (char '>') $ many (satisfy $ \x -> x >= '\x20' && x <= '\x7e' && x /= '\x3e') >> error "Prose elements not supported"
{-

Crocker & Overell           Standards Track                    [Page 11]
 
RFC 5234                          ABNF                      January 2008


         prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
                                ; bracketed string of SP and VCHAR
                                ;  without angles
                                ; prose description, to be used as
                                ;  last resort
-}

{-

let abnf_abnf = System.IO.Unsafe.unsafePerformIO $ System.IO.readFile "../abnf.abnf"

-}
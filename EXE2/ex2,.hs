data Gibi = Nome String
            |LogoEditora String String
            |Leitor Pessoa Bool
            deriving Show

data Pessoa = Pessoa String String Genero
            deriving Show            

data Genero = Masculino | Feminino | Naoespecificado
            deriving Show   

nomeLeitor :: Gibi -> String
nomeLeitor c = case c of
                     Nome nom                     -> nom 
                     LogoEditora log ed                -> log ++ " " ++ ed
                     Leitor p promo                   ->
                        case p of
                            Pessoa prnom sbrnom ge          -> prnom ++ " " ++ sbrnom  

nomeLeitor' :: Gibi -> String
nomeLeitor'  (Nome nom)                    = nom
nomeLeitor'  (LogoEditora log ed)              = log ++ " " ++ ed
nomeLeitor'  (Leitor(Pessoa prnom sbrnom _) promo)    =  prnom ++ " " ++ sbrnom
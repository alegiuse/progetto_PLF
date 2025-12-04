-- ##########################################################
-- #    Corso di Programmazione Logica e Funzionale         #
-- #  Progetto per la sessione autunnale A.A. 2024/2025     #
-- #       di Nasrine Aboufaris, Matricola: 321885          #
-- #       e Alessia Giuseppetti, Matricola: 322984         #
-- #                Anno di corso: Terzo                    #
-- ##########################################################

{- Specifica: Programma Haskell per la codifica/decodifica
di codici di Hamming e calcolo della distanza di Hamming -}

import Data.Bits (testBit, xor, (.&.), shiftL, finiteBitSize)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.List (foldl')

-- DEFINIZIONE DEI TIPI

type Bit = Int
type ParolaBinaria = [Bit]

{- Azione main
   Descrizione: punto di ingresso del programma. 
   Stampa il benvenuto e avvia il ciclo principale.-}
main :: IO ()
main = do
    mapM_ putStrLn messaggioBenvenuto
    cicloPrincipale

{- Azione cicloPrincipale
   Descrizione: gestisce il menu interattivo e lo smistamento delle operazioni.
   Ricorsione:
     - Caso Base: l'utente inserisce "4" (termina l'esecuzione).
     - Passo Ricorsivo: l'utente inserisce "1", "2", "3" 
                        o un input non valido (l'azione richiama se stessa).-}
cicloPrincipale :: IO ()
cicloPrincipale = do
    putStrLn "\nScegliere l'operazione:"
    mapM_ putStrLn vociMenu
    scelta <- getLine
    case scelta of
        "1" -> gestioneCodifica   >> cicloPrincipale
        "2" -> gestioneDecodifica >> cicloPrincipale
        "3" -> gestioneDistanza   >> cicloPrincipale
        "4" -> putStrLn "\nEsecuzione terminata"
        _   -> putStrLn "Opzione non valida. Riprovare." >> cicloPrincipale

{- Azione gestioneCodifica
   Descrizione: gestisce il flusso per l'operazione di codifica.-}
gestioneCodifica :: IO ()
gestioneCodifica = do
    putStrLn "\nCodifica di Hamming"
    m <- richiediM
    let (n, k) = calcolaParametriHamming m
    putStrLn $ "Parametri: n = " ++ show n ++ ", k = " ++ show k
    dati <- richiediParola k
    let codificata = codificaHamming m dati
    putStrLn $ "Parola codificata: " ++ concatMap show codificata

{- Azione gestioneDecodifica
   Descrizione: gestisce il flusso per l'operazione di decodifica.-}
gestioneDecodifica :: IO ()
gestioneDecodifica = do
    putStrLn "\nDecodifica di Hamming"
    m <- richiediM
    let (n, k) = calcolaParametriHamming m
    putStrLn $ "Parametri: n = " ++ show n ++ ", k = " ++ show k
    ricevuta <- richiediParola n
    let (decodificata, possibileErrore) = decodificaHamming m ricevuta
    case possibileErrore of
        Nothing ->
            putStrLn $ "Nessun errore rilevato.\nParola decodificata: " 
                        ++ concatMap show decodificata
        Just posizione ->
            putStrLn $ "Errore corretto in posizione " ++ show posizione ++
                       "\nParola decodificata: " ++ concatMap show decodificata

{- Azione gestioneDistanza
   Descrizione: gestisce il flusso per il calcolo della distanza di Hamming.-}
gestioneDistanza :: IO ()
gestioneDistanza = do
    putStrLn "\nDistanza di Hamming"
    parola1 <- elaboraInput (\s -> validaParolaBinaria (length s) s)
               ("Inserire la prima parola binaria\n" ++ init (unlines mostraIstruzioniInput))
               "Errore: inserire parola di soli 0 e 1.\n"

    parola2 <- elaboraInput (validaParolaBinaria (length parola1))
              ("Inserire la seconda parola binaria (lunghezza " ++ show (length parola1) ++ ")\n" 
              ++ init(unlines mostraIstruzioniInput))
              "Errore: lunghezza diversa dalla prima parola o caratteri non validi.\n"

    let distanza = calcolaDistanzaHamming parola1 parola2
    putStrLn $ "Distanza di Hamming: " ++ show distanza

{- Azione elaboraInput
   Descrizione: gestore generico dell'input utente con validazione.
   Ricorsione:
     - Caso Base: il 'validatore' restituisce l'input valido.
     - Passo Ricorsivo: il 'validatore' restituisce 'Nothing'. 
                        Stampa errore e richiama se stessa.-}
elaboraInput :: (String -> Maybe a) -> String -> String -> IO a
elaboraInput validatore richiesta msgErrore = do
    putStrLn richiesta
    ingresso <- getLine
    case validatore ingresso of
        Just valore -> return valore
        Nothing  -> putStrLn msgErrore >> elaboraInput validatore richiesta msgErrore

{- Azione richiediM
   Descrizione: richiede all'utente il parametro 'm' (bit di parità).-}
richiediM :: IO Int
richiediM = elaboraInput validaM
     ("Il numero di bit di parità deve essere maggiore o uguale a 2"++ 
      " e minore del limite di sistema pari a " ++ show limiteM ++
      "\nInserire il numero di bit di parità scelto:")
    "Errore: valore non valido.\n"

{- Azione richiediParola
   Descrizione: richiede all'utente una parola binaria di lunghezza specifica.-}
richiediParola :: Int -> IO ParolaBinaria
richiediParola lun = elaboraInput (validaParolaBinaria lun)
    ("Inserire parola binaria di lunghezza " ++ show lun ++ "\n" 
    ++ init (unlines mostraIstruzioniInput))
    ("Errore: devi inserire esattamente " ++ show lun ++ " bit (0 o 1).\n")

-- LOGICA DI HAMMING 

{- Funzione codificaHamming
   Descrizione: coordina la costruzione della parola e l'inserimento parità.
   Argomenti: m (numero bit parità), dati (parola originale).-}
codificaHamming :: Int -> ParolaBinaria -> ParolaBinaria
codificaHamming m dati =
    let (n, _) = calcolaParametriHamming m
        struttura = costruisciParola n dati
    in inserisciParitaRicorsiva m 0 struttura

{- Funzione decodificaHamming
   Descrizione: decodifica usando il calcolo ricorsivo della sindrome 
                e corregge eventuale errore.
   Argomenti: m (numero bit parità), ricevuta (parola da decodificare).-}
decodificaHamming :: Int -> ParolaBinaria -> (ParolaBinaria, Maybe Int)
decodificaHamming m ricevuta =
    case calcolaSindrome m 0 ricevuta of
        0 -> (estraiDati ricevuta, Nothing)
        s -> let corretta = correggiBit ricevuta (s - 1)
             in (estraiDati corretta, Just s)

{- Funzione calcolaDistanzaHamming
   Descrizione: calcola la distanza confrontando le teste delle liste ricorsivamente.
   Argomenti: due parole binarie (x:xs) e (y:ys).
   Ricorsione: 
     - Caso Base: liste vuote -> distanza 0.
     - Passo Ricorsivo: confronta teste (XOR) + chiamata ricorsiva sulle code.-}
calcolaDistanzaHamming :: ParolaBinaria -> ParolaBinaria -> Int
calcolaDistanzaHamming [] [] = 0 
calcolaDistanzaHamming (x:xs) (y:ys) = 
    (x `xor` y) + calcolaDistanzaHamming xs ys

-- FUNZIONI AUSILIARIE E MATEMATICHE

{- Funzione calcolaSindrome
   Descrizione: calcola la sindrome sommando i contributi dei bit di controllo errati.
   Argomenti: m (limite), i (corrente), parola.
   Ricorsione: Sì
     - Caso Base: Quando i >= m, non ci sono più controlli da fare -> restituisce 0.
     - Passo Ricorsivo: 
         1. Calcola il bit di parità per la posizione corrente 'i'.
         2. Calcola il contributo alla sindrome: (bitCalcolato * 2^i).
         3. Somma il contributo corrente al risultato della chiamata ricorsiva su (i + 1).-}
calcolaSindrome :: Int -> Int -> ParolaBinaria -> Int
calcolaSindrome m i parola
    | i >= m = 0 
    | otherwise = 
        let bitCalcolato = calcolaBitParitaSpecifico i parola
            valoreSindrome = bitCalcolato * (1 `shiftL` i)
        in valoreSindrome + calcolaSindrome m (i + 1) parola

{- Funzione inserisciParitaRicorsiva
   Descrizione: calcola e inserisce i bit di parità in modo ricorsivo.
   Argomenti: m (totale parità), i (indice corrente), parola.
   Ricorsione: 
     - Caso Base: i >= m (tutti i bit calcolati), restituisce la parola.
     - Passo Ricorsivo: calcola parità per i, aggiorna la lista, ricorre su i + 1.-}
inserisciParitaRicorsiva :: Int -> Int -> ParolaBinaria -> ParolaBinaria
inserisciParitaRicorsiva m i parola
    | i >= m = parola 
    | otherwise =    
        let posizioneParita = (1 `shiftL` i) - 1
            bitCalcolato = calcolaBitParitaSpecifico i parola
            -- Aggiorna la lista
            (prima, _:dopo) = splitAt posizioneParita parola
            nuovaParola = prima ++ bitCalcolato : dopo
        in inserisciParitaRicorsiva m (i + 1) nuovaParola

{- Funzione costruisciParola
   Descrizione: Inserisce i bit informativi nelle posizioni non potenze di 2.
   Ricorsione: 
     - Caso Base: posizione > n, restituisce lista vuota.
     - Passo Ricorsivo: inserisce 0 o bit dati e richiama su posizione + 1.-}
costruisciParola :: Int -> ParolaBinaria -> ParolaBinaria
costruisciParola n dati = scorri 1 dati
  where
    scorri posizione restoDati
        | posizione > n = []
        | potenzaDiDue posizione = 0 : scorri (posizione + 1) restoDati
        | otherwise = case restoDati of
            (d:ds) -> d : scorri (posizione + 1) ds
            []     -> []

{- Funzione calcolaBitParitaSpecifico
   Descrizione: calcola il bit di parità per una specifica posizione 'i' (esponente).-}
calcolaBitParitaSpecifico :: Int -> ParolaBinaria -> Bit
calcolaBitParitaSpecifico i parola =
    foldl' xor 0 [b | (posizione, b) <- zip [(1::Int)..] parola, testBit posizione i]

{- Funzione validaM
   Descrizione: valida se la stringa in ingresso rappresenta un intero 'm' valido.-}
validaM :: String -> Maybe Int
validaM s = case readMaybe s of
    Just m | m >= 2 && m <= limiteM -> Just m
    _                               -> Nothing

{- Funzione validaParolaBinaria
   Descrizione: converte stringa in ParolaBinaria se lunghezza e contenuto sono validi.-}
validaParolaBinaria :: Int -> String -> Maybe ParolaBinaria
validaParolaBinaria lun s
    | length s /= lun = Nothing
    | otherwise = traverse carattereInBit s
  where
    carattereInBit '0' = Just 0
    carattereInBit '1' = Just 1
    carattereInBit _   = Nothing

{- Funzione calcolaParametriHamming
   Descrizione: calcola n (lunghezza totale) e k (lunghezza dati) dato m.-}
calcolaParametriHamming :: Int -> (Int, Int)
calcolaParametriHamming m = (n, n - m)
    where n = (1 `shiftL` m) - 1

{- Funzione potenzaDiDue
   Descrizione: verifica se un intero è potenza di due.-}
potenzaDiDue :: Int -> Bool
potenzaDiDue n = n > 0 && (n .&. (n - 1)) == 0

{- Funzione correggiBit
   Descrizione: inverte il bit alla posizione indicata.-}
correggiBit :: ParolaBinaria -> Int -> ParolaBinaria
correggiBit parola indice
    | indice < 0 || indice >= length parola = parola
    | otherwise =
        let (prima, b:dopo) = splitAt indice parola
        in prima ++ (1 - b) : dopo

{- Funzione estraiDati
   Descrizione: rimuove i bit di controllo (potenze di 2).-}
estraiDati :: ParolaBinaria -> ParolaBinaria
estraiDati parola = [b | (posizione, b) <- zip [1..] parola, not (potenzaDiDue posizione)]

-- DATI COSTANTI

{- Dato costante limiteM
   Descrizione: limite massimo per m basato sull'architettura.-}
limiteM :: Int
limiteM = finiteBitSize (0 :: Int) - 2

{- Dato costante messaggioBenvenuto
   Descrizione: restituisce la lista di stringhe per il messaggio di benvenuto.-}
messaggioBenvenuto :: [String]
messaggioBenvenuto =
    [ "\n------------------------------------Benvenuto------------------------------------"
    , "Il seguente programma permette di utilizzare i codici di Hamming generici (n,k)"
    , "per la codifica, decodifica e calcolo della distanza di Hamming tra due parole"
    , "binarie."
    , "Si ricorda che la decodifica implementata permette di individuare e correggere"
    , "un singolo errore."
    , "Istruzioni:"
    , "Scegliere dal menu una delle tre operazioni da eseguire."
    , "Per la codifica e decodifica di Hamming: "
    , " 1. Inserire l'indice di parità (numero intero >= 2)"
    , " 2. Inserire una parola binaria di lunghezza adeguata ai parametri del codice di"
    , "    Hamming calcolati in base a m."
    , "Per il calcolo della distanza di Hamming: "
    , " 1. Inserire due parole binarie della stessa lunghezza" 
    ]
    ++ mostraIstruzioniInput ++
    [ "L\'esecuzione termina selezionando l\'apposita voce dal menu."
    , "---------------------------------------------------------------------------------"
    ]

{- DatoCostante vociMenu
   Descrizione: lista delle opzioni del menu.-}
vociMenu :: [String]
vociMenu =
    [ "1. Codifica di Hamming"
    , "2. Decodifica di Hamming"
    , "3. Distanza di Hamming"
    , "4. Esci"
    ]

{- Dato costante mostraIstruzioniInput
   Descrizione: istruzioni di formattazione per l'input utente.-}
mostraIstruzioniInput :: [String]
mostraIstruzioniInput = 
    [ "Nota: le parole devono essere inserite nel seguente formato, es. 1011"]
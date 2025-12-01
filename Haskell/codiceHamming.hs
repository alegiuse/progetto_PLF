-- ##########################################################
-- #    Corso di Programmazione Logica e Funzionale         #
-- #  Progetto per la sessione autunnale A.A. 2024/2025     #
-- #       di Nasrine Aboufaris, Matricola: 321885          #
-- #       e Alessia Giuseppetti, Matricola: 322984         #
-- #                 Anno di corso: Terzo                   #
-- ##########################################################

{- Specifica: Scrivere un programma Haskell che acquisisce da tastiera un messaggio binario, 
il quale verrà codificato o decodificato mediante un codice di Hamming generico (n,k), in base 
alla scelta dell'utente. I parametri (n,k) saranno derivati da un valore m, fornito dall'utente, 
che rappresenta il numero di bit di parità. Il programma sarà inoltre in grado di calcolare la 
distanza di Hamming tra due stringhe binarie, fornite dall'utente, di lunghezza uguale ma arbitraria.-}

import Data.Bits (testBit, xor, (.&.), shiftL, finiteBitSize)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.List (foldl')

-- Tipi di dati
type Bit = Int
type ParolaBinaria = [Bit]

{- Funzione limiteM costante
   Descrizione: definisce il limite massimo per m basandosi sulla dimensione dei bit del sistema. -}
limiteM :: Int
limiteM = finiteBitSize (0 :: Int) - 2

{- Funzione potenzaDiDue
   Descrizione: determina se un numero intero è una potenza di due.
   Argomenti: n (numero intero da verificare). -}
potenzaDiDue :: Int -> Bool
potenzaDiDue n = n > 0 && (n .&. (n - 1)) == 0

{- Funzione calcolaParametriHamming 
   Descrizione: calcola la lunghezza totale del codice (n) e la lunghezza dei dati (k) dato il numero di bit di parità (m).
   Argomenti: m (numero di bit di parità). -}
calcolaParametriHamming :: Int -> (Int, Int)
calcolaParametriHamming m = (n, n - m)
    where n = (1 `shiftL` m) - 1

{- Funzione costruisciStruttura 
   Descrizione: inserisce placeholder (zeri) nelle posizioni che sono potenze di due, preparando la struttura per i bit di parità.
   Argomenti: n (lunghezza totale), dati (lista di bit di dati).
   Casi (nella helper 'go'):
     - Base: pos > n, restituisce lista vuota.
     - Generale 1: pos è potenza di due, inserisce 0 e ricorre.
     - Generale 2: altrimenti, inserisce il prossimo bit dati e ricorre. -}
costruisciStruttura :: Int -> ParolaBinaria -> ParolaBinaria
costruisciStruttura n dati = go 1 dati
  where
    go pos xs
        | pos > n = []
        | potenzaDiDue pos = 0 : go (pos + 1) xs
        | otherwise = case xs of
            (d:ds) -> d : go (pos + 1) ds
            []     -> []

{- Funzione calcolaBitParita
   Descrizione: calcola il valore di un singolo bit di parità per una specifica posizione di controllo.
   Argomenti: i (indice dell'esponente 2^i), parola (la sequenza binaria). -}
calcolaBitParita :: Int -> ParolaBinaria -> Bit
calcolaBitParita i parola =
    foldl' xor 0 [b | (pos, b) <- zip [(1::Int)..] parola, testBit pos i]

{- Funzione inserisciParita
   Descrizione: sostituisce i placeholder (zeri) nelle posizioni di parità con i bit di parità calcolati.
   Argomenti: m (numero bit parità), struttura (parola con placeholder). -}
inserisciParita :: Int -> ParolaBinaria -> ParolaBinaria
inserisciParita m struttura = foldl' inserisci struttura [0..m-1]
  where
    inserisci parolaTemp i =
        let posPar = (1 `shiftL` i) - 1
            bitPar = calcolaBitParita i parolaTemp
        in aggiornaPos parolaTemp posPar bitPar
    
    aggiornaPos xs idx val =
        let (before, _:after) = splitAt idx xs
        in before ++ val : after

{- Funzione codificaHamming
   Descrizione: esegue l'intero processo di codifica Hamming (calcolo parametri, struttura, inserimento parità).
   Argomenti: m (numero bit parità), dati (lista bit input). -}
codificaHamming :: Int -> ParolaBinaria -> ParolaBinaria
codificaHamming m dati =
    let (n, _) = calcolaParametriHamming m
        struttura = costruisciStruttura n dati
    in inserisciParita m struttura

{- Funzione calcolaSindrome
   Descrizione: calcola la sindrome sommando le posizioni dove la parità non corrisponde.
   Argomenti: m (numero bit parità), parola (parola ricevuta). -}
calcolaSindrome :: Int -> ParolaBinaria -> Int
calcolaSindrome m parola =
    sum [valXor * (1 `shiftL` i) | i <- [0..m-1], let valXor = calcolaBitParita i parola, valXor /= 0]

{- Funzione correggiBit
   Descrizione: inverte il bit alla posizione specificata per correggere l'errore.
   Argomenti: parola (lista bit), idx (indice 0-based del bit da invertire). -}
correggiBit :: ParolaBinaria -> Int -> ParolaBinaria
correggiBit parola idx
    | idx < 0 || idx >= length parola = parola
    | otherwise =
        let (before, b:after) = splitAt idx parola
        in before ++ (1 - b) : after

{- Funzione estraiDati
   Descrizione: rimuove i bit di parità (posizioni potenza di due) restituendo solo i dati originali.
   Argomenti: parola (lista bit completa). -}
estraiDati :: ParolaBinaria -> ParolaBinaria
estraiDati parola = [b | (pos, b) <- zip [1..] parola, not (potenzaDiDue pos)]

{- Funzione decodificaHamming
   Descrizione: gestisce la decodifica, rileva errori tramite sindrome, corregge se necessario ed estrae i dati.
   Argomenti: m (numero bit parità), ricevuta (parola ricevuta). -}
decodificaHamming :: Int -> ParolaBinaria -> (ParolaBinaria, Maybe Int)
decodificaHamming m ricevuta =
    case calcolaSindrome m ricevuta of
        0 -> (estraiDati ricevuta, Nothing)
        s -> let corretta = correggiBit ricevuta (s - 1)
             in (estraiDati corretta, Just s)

{- Funzione calcolaDistanzaHamming
   Descrizione: calcola la distanza di Hamming (numero di bit diversi) tra due parole.
   Argomenti: p1, p2 (due liste di bit da confrontare). -}
calcolaDistanzaHamming :: ParolaBinaria -> ParolaBinaria -> Int
calcolaDistanzaHamming p1 p2 = sum [1 | (a, b) <- zip p1 p2, a /= b]

{- Funzione validaM
   Descrizione: valida se l'input stringa per 'm' è un intero valido entro i limiti.
   Argomenti: s (stringa input). -}
validaM :: String -> Maybe Int
validaM s = do
    m <- readMaybe s
    if m >= 2 && m <= limiteM then Just m else Nothing

{- Funzione validaParolaBinaria
   Descrizione: Converte una stringa di '0' e '1' in una lista di Bit, verificando la lunghezza.
   Argomenti: len (lunghezza attesa), s (stringa input). -}
validaParolaBinaria :: Int -> String -> Maybe ParolaBinaria
validaParolaBinaria len s
    | length s /= len = Nothing
    | otherwise = traverse charToBit s
  where
    charToBit '0' = Just 0
    charToBit '1' = Just 1
    charToBit _   = Nothing

{- Funzione (Dato Costante)
   Descrizione: Lista di stringhe contenente il testo di benvenuto.
   Argomenti: Nessuno. -}
messaggioBenvenuto :: [String]
messaggioBenvenuto =
    [ "\n------------------------------------Benvenuto------------------------------------"
    , "Il seguente programma permette di utilizzare i codici di Hamming generici (n,k)"
    , "per la codifica, decodifica e calcolo della distanza di Hamming tra due parole"
    , "binarie."
    , "Si ricorda che la decodifica di Hamming implementata permette solo di individuare"
    , "e correggere un singolo errore."
    , "Come funziona il programma:"
    , "Potrai scegliere dal menu una delle tre operazioni da eseguire."
    , "Per la codifica e decodifica di Hamming: "
    , "1. Ti verrà richiesto di inserire l'indice di parità (numero intero <= 2)"
    , "2. n e k verranno calcolati automaticamente"
    , "3. Ti verrà chiesto di inserire una parola binaria con una determinata lunghezza"
    , "Per la distanza di Hamming, ti verrà richiesto di inserire due parole di esatta"
    , "lunghezza."
    , "Esempio di parola accettata '1011'."
    , "Il programma finisce solo nel momento in cui si sceglie l'opzione esci dal menu"
    , "---------------------------------------------------------------------------------"
    ]

{- Tipo: Funzione (Dato Costante)
   Descrizione: Lista delle opzioni del menu.
   Argomenti: Nessuno. -}
vociMenu :: [String]
vociMenu =
    [ "1. Codifica di Hamming"
    , "2. Decodifica di Hamming"
    , "3. Distanza di Hamming"
    , "4. Esci"
    ]

{- Azione elaboraInput
   Descrizione: gestisce l'input utente generico con validazione e messaggi di errore (ricorsiva in caso di errore).
   Argomenti: validatore (funzione di check), prompt (messaggio richiesta), msgErrore (messaggio fallimento). -}
elaboraInput :: (String -> Maybe a) -> String -> String -> IO a
elaboraInput validatore prompt msgErrore = do
    putStrLn prompt
    input <- getLine
    case validatore input of
        Just val -> return val
        Nothing  -> putStrLn msgErrore >> elaboraInput validatore prompt msgErrore

{- Azione richiediM
   Descrizione: wrapper per richiedere specificamente il parametro m (bit di parità). -}
richiediM :: IO Int
richiediM = elaboraInput validaM
     ("Il bit di parità deve essere maggiore o uguale a 2 e minore del limite di sistema pari a " ++ show (finiteBitSize (0::Int) - 2) ++
    "\nInserire il bit di parità scelto:")
    "Errore: valore non valido.\n"

{- Azione richiediParola
   Descrizione: Wrapper per richiedere una parola binaria di lunghezza specifica.
   Argomenti: len (lunghezza richiesta). -}
richiediParola :: Int -> IO ParolaBinaria
richiediParola len = elaboraInput (validaParolaBinaria len)
    ("Inserisci parola binaria di lunghezza " ++ show len ++ ":")
    ("Errore: devi inserire esattamente " ++ show len ++ " bit (0 o 1).")

{- Azione gestioneCodifica
   Descrizione: Gestisce il flusso dell'operazione di codifica (input m, calcolo k, input parola, stampa risultato).
   Argomenti: Nessuno. -}
gestioneCodifica :: IO ()
gestioneCodifica = do
    putStrLn "\nCodifica di Hamming"
    m <- richiediM
    let (n, k) =  calcolaParametriHamming m
    putStrLn $ "Parametri: n = " ++ show n ++ ", k = " ++ show k
    dati <- richiediParola k
    let codificata = codificaHamming m dati
    putStrLn $ "Parola codificata: " ++ concatMap show codificata

{- Azione gestioneDecodifica
   Descrizione: Gestisce il flusso dell'operazione di decodifica (input m, input parola, visualizzazione correzione).
   Argomenti: Nessuno. -}
gestioneDecodifica :: IO ()
gestioneDecodifica = do
    putStrLn "\nDecodifica di Hamming"
    m <- richiediM
    let (n, k) =  calcolaParametriHamming m
    putStrLn $ "Parametri: n = " ++ show n ++ ", k = " ++ show k
    ricevuta <- richiediParola n
    let (decodificata, mbErrore) = decodificaHamming m ricevuta
    case mbErrore of
        Nothing ->
            putStrLn $ "Nessun errore rilevato.\nParola decodificata: " ++ concatMap show decodificata
        Just pos ->
            putStrLn $ "Errore corretto in posizione " ++ show pos ++
                       "\nParola decodificata: " ++ concatMap show decodificata

{- Azione gestioneDistanza
   Descrizione: Gestisce il flusso per il calcolo della distanza tra due parole binarie inserite dall'utente.
   Argomenti: Nessuno. -}
gestioneDistanza :: IO ()
gestioneDistanza = do
    putStrLn "\nDistanza di Hamming"
    p1 <- elaboraInput (\s -> validaParolaBinaria (length s) s)
              "Inserisci la prima parola binaria:"
              "Errore: parola non valida (usa solo 0 e 1).\n"

    p2 <- elaboraInput (validaParolaBinaria (length p1))
              ("Inserisci la seconda parola (lunghezza " ++ show (length p1) ++ "):")
              "Errore: lunghezza diversa dalla prima o caratteri non validi.\n"

    let distanza = calcolaDistanzaHamming p1 p2
    putStrLn $ "Distanza di Hamming: " ++ show distanza

{- Azione cicloPrincipale
   Descrizione: ciclo principale del programma che mostra il menu e smista le operazioni.
   Casi:
     - "1", "2", "3": Esegue gestione specifica e ricorre.
     - "4": Termina (caso base implicito).
     - Altro: Messaggio errore e ricorre. -}
cicloPrincipale :: IO ()
cicloPrincipale = do
    putStrLn "\nScegliere l'operazione desiderata:"
    mapM_ putStrLn vociMenu
    scelta <- getLine
    case scelta of
        "1" -> gestioneCodifica   >> cicloPrincipale
        "2" -> gestioneDecodifica >> cicloPrincipale
        "3" -> gestioneDistanza   >> cicloPrincipale
        "4" -> putStrLn "\nGrazie per aver utilizzato il programma. Arrivederci!"
        _   -> putStrLn "Opzione non valida. Riprovare." >> cicloPrincipale

{- Azione main
   Descrizione: Entry point del programma, stampa il benvenuto e avvia il ciclo principale. -}
main :: IO ()
main = do
    mapM_ putStrLn messaggioBenvenuto
    cicloPrincipale
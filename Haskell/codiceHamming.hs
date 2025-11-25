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

import Data.Bits (testBit, xor, (.&.), shiftL)
import Text.Read (readMaybe)
import Control.Monad (unless, when)
import Data.List (foldl')
import System.Exit (exitSuccess)

-- Tipi di dati per migliorare la leggibilità del codice 
type Bit = Int
type ParolaBinaria = [Bit]

{- main: Funzione principale del programma -}
main :: IO ()
main = do 
    print "------------------------------------Benvenuto------------------------------------"
    print "Il seguente programma permette di utilizzare i codici di Hamming generici (n,k)"
    print "per la codifica, decodifica e calcolo della distanza di Hamming tra due parole"
    print "binarie."
    print "Come funziona il programma:"
    print "Potrai scegliere dal menu una delle tre operazioni da eseguire, per la codifica"
    print "e decodifica di Hamming, verra\' chiesto di inserire m, ovvero il numero di bit "
    print "di parita\', grazie ad m verranno calcolati n e k, e si potra\' inserire la"
    print "parola binaria. Esempio di parola accetta '1011'"
    print "Notare che per la decodifica questo algoritmo corregge solo errori singoli."
    print "Con errori multipli il risultato potrebbe essere errato."
    print "Il programma finisce solo nel momento in cui si sceglie l'opzione esci dal menu"
    print "---------------------------------------------------------------------------------"
    cicloPrincipale

{- cicloPrincipale: Gestisce il menu principale e le scelte dell'utente -}
cicloPrincipale :: IO ()
cicloPrincipale = do
    -- Lista delle operazioni disponibili nel programma 
    let vociMenu = [ "Codifica di Hamming"
                   , "Decodifica di Hamming"
                   , "Distanza di Hamming"
                   , "Esci"
                   ]
    
    -- Stampa il menu usando mapM_ per eseguire l'azione su ogni elemento 
    putStrLn "\nScegli operazione:"
    mapM_ (\(indice, voce) -> putStrLn $ show indice ++ " - " ++ voce) 
          (zip [1..] vociMenu)
    
    -- Pattern matching sull'input dell'utente per determinare l'azione 
    scelta <- getLine
    case scelta of
        "1" -> gestioneCodificaHamming >> cicloPrincipale
        "2" -> gestioneDecodificaHamming >> cicloPrincipale
        "3" -> gestioneDistanzaHamming >> cicloPrincipale
        "4" -> exitSuccess
        _   -> do
            putStrLn "Scelta non valida. Riprova."
            cicloPrincipale 

{- gestioneCodificaHamming: Gestisce il processo di codifica di Hamming
   coordina input utente, calcoli e output per la codifica -}
gestioneCodificaHamming :: IO()
gestioneCodificaHamming = do
    putStrLn "\nCodifica di Hamming"
    parametroM <- controlloParametroM 
    -- Calcola automaticamente n e k basandosi su m 
    let (parametroN, parametroK) = calcolaParametriHamming parametroM
    putStrLn $ "Parametri calcolati: n = " ++ show parametroN ++ ", k = " ++ show parametroK
    parola <- controlloParolaBinaria parametroK
    {- Converte la stringa in lista di bit usando map e read -}
    let parolaDati = map (read . (:[])) parola :: ParolaBinaria
    let parolaCodificata = codificaHamming parametroM parolaDati
    putStrLn $ "La parola codificata è: " ++ concatMap show parolaCodificata

{- gestioneDecodificaHamming: Gestisce il processo di decodifica di Hamming
   coordina input utente, calcoli e output per la decodifica -}
gestioneDecodificaHamming :: IO()
gestioneDecodificaHamming = do 
    putStrLn "\nDecodifica di Hamming"
    parametroM <- controlloParametroM 
    let (parametroN, parametroK) = calcolaParametriHamming parametroM
    putStrLn $ "Parametri calcolati: n = " ++ show parametroN ++ ", k = " ++ show parametroK
    parola <- controlloParolaBinaria parametroN
    let parolaRicevuta = map (read . (:[])) parola :: ParolaBinaria
    let (parolaDecodificata, errore) = decodificaHamming parametroM parolaRicevuta
    -- Pattern matching su Maybe per gestire presenza o assenza di errore 
    case errore of
        Nothing -> 
            putStrLn $ "Nessun errore rilevato. La parola decodificata è: " ++ concatMap show parolaDecodificata
        Just posizione -> 
            putStrLn $ "Corretto un errore nella posizione " ++ show posizione ++ 
                      ". La parola decodificata è: " ++ concatMap show parolaDecodificata

{- gestioneDistanzaHamming: Gestisce il calcolo della distanza di Hamming
   coordina input di due parole e calcolo della distanza -}
gestioneDistanzaHamming :: IO()
gestioneDistanzaHamming = do 
    putStrLn "\nDistanza di Hamming"
    (s1, s2) <- controlloDueParole
    let p1 = map (read . (:[])) s1 :: ParolaBinaria
    let p2 = map (read . (:[])) s2 :: ParolaBinaria
    putStrLn $ "La distanza di Hamming tra le due parole è: " ++ show (calcolaDistanzaHamming p1 p2)

{- controlloParametroM: Valida l'input del parametro m
   continua a chiedere input fino a ottenere un valore valido -}
controlloParametroM :: IO Int 
controlloParametroM = do 
    putStrLn "Inserisci m (m >= 2):"
    input <- getLine
    -- Usa readMaybe per parsing sicuro che ritorna Maybe Int 
    case readMaybe input of
        Just m | m >= 2 -> return m
        _ -> do
            putStrLn "Errore: m deve essere un intero maggiore uguale a 2"
            controlloParametroM

{- controlloParolaBinaria: Valida l'input di una parola binaria di lunghezza specifica
   Argomenti: lunghezza richiesta della parola
   continua a chiedere input fino a ottenere una parola valida -}
controlloParolaBinaria :: Int -> IO String
controlloParolaBinaria lunghezza = do
    putStrLn $ "Inserisci parola binaria di lunghezza " ++ show lunghezza ++ ":"
    parola <- getLine
    -- all verifica che tutti i caratteri siano in "01" 
    if all (`elem` "01") parola && length parola == lunghezza
        then return parola
        else do
            putStrLn "Errore: lunghezza non valida o caratteri non binari"
            controlloParolaBinaria lunghezza

{- controlloDueParole: Valida l'input di due parole binarie di uguale lunghezza
   continua a chiedere input fino a ottenere due parole valide -}
controlloDueParole :: IO (String, String)
controlloDueParole = do
    putStrLn "Inserisci prima parola binaria:"
    p1 <- getLine
    putStrLn "Inserisci seconda parola binaria (stessa lunghezza):"
    p2 <- getLine
    if length p1 == length p2 && all (`elem` "01") p1 && all (`elem` "01") p2
        then return (p1, p2)
        else do
            putStrLn "Errore: le parole devono essere binarie e di uguale lunghezza"
            controlloDueParole

{- calcolaParametriHamming: Calcola i parametri n e k del codice di Hamming
   Argomenti: m (numero di bit di parità)
   calcola n = 2^m - 1 e k = n - m -}
calcolaParametriHamming :: Int -> (Int, Int)
calcolaParametriHamming m = (n, n - m)
    -- where permette di definire n localmente per riutilizzarlo 
    where n = 2 ^ m - 1

{- potenzaDiDue: Verifica se un numero è una potenza di 2
   Argomenti: n (numero da verificare) -}
potenzaDiDue :: Int -> Bool
potenzaDiDue n = n /= 0 && (n .&. (n-1)) == 0

{- aggiornaElemento: Aggiorna un elemento in una lista a un indice specifico
   Argomenti: lista, indice, nuovo valore
   divide la lista e ricostruisce con nuovo valore -}
aggiornaElemento :: [a] -> Int -> a -> [a]
aggiornaElemento lista indice nuovo = 
    -- splitAt divide la lista in due parti all'indice specificato 
    let (prima, _:dopo) = splitAt indice lista
    in prima ++ [nuovo] ++ dopo

{- codificaHamming: Codifica una parola usando il codice di Hamming
   Argomenti: m (bit di parità), parolaDati (dati da codificare)
   crea parola iniziale e inserisce dati nelle posizioni corrette
   coordina inserimento dati e calcolo bit di parità -}
codificaHamming :: Int -> ParolaBinaria -> ParolaBinaria
codificaHamming m parolaDati =
    let (n, _) = calcolaParametriHamming m
        -- Inizializza una parola di tutti zeri 
        parolaIniziale = replicate n 0
        indiciNonParita = filter (\j -> not (potenzaDiDue (j+1))) [0..n-1]
        parolaConDati = inserisciDati parolaIniziale indiciNonParita parolaDati
    in calcolaParita parolaConDati n m

{- calcolaParita: Calcola tutti i bit di parità per una parola
   Argomenti: parola, n (lunghezza), m (numero bit parità)
   Casi base: quando lista [0..m-1] è vuota ritorna parola invariata
   Casi generali: per ogni i in [0..m-1] calcola il bit di parità i-esimo
   usa foldl' per applicare calcolaEAggiornaParita a ogni posizione di parità -}
calcolaParita :: ParolaBinaria -> Int -> Int -> ParolaBinaria
calcolaParita parola n m = foldl' (calcolaEAggiornaParita n) parola [0..m-1]


{- calcolaEAggiornaParita: Calcola e aggiorna un singolo bit di parità
   Argomenti: n (lunghezza parola), parola, i (indice bit parità)
   identifica bit controllati, calcola XOR e aggiorna posizione -}
calcolaEAggiornaParita :: Int -> ParolaBinaria -> Int -> ParolaBinaria
calcolaEAggiornaParita n parola i =
    let posizioneParita = 2^i
        indiceParita = posizioneParita - 1
        -- testBit j i verifica se il bit i-esimo di j è settato 
        indiciControllati = [ j-1 | j <- [1..n], testBit j i ]
        -- Esclude il bit de parità stesso dal calcolo -}
        bitDati = map (parola !!) (filter (/= indiceParita) indiciControllati)
        -- XOR di tutti i bit controllati 
        valoreXOR = foldl' xor 0 bitDati
    in aggiornaElemento parola indiceParita valoreXOR

{- inserisciDati: Inserisce i dati nelle posizioni specificate
   Argomenti: parola iniziale, lista indici, lista dati
   Casi base: quando liste indici e dati sono vuote ritorna parola invariata
   Casi generali: per ogni coppia (indice, dato) aggiorna la parola
   usa foldl' per applicare aggiornaElemento a ogni coppia -}
inserisciDati :: [a] -> [Int] -> [a] -> [a]
inserisciDati parola indici dati = 
    -- zip associa ogni indice al dato corrispondente
    foldl' (\acc (indice, dato) -> aggiornaElemento acc indice dato) parola (zip indici dati)

{- calcolaSindrome: Calcola la sindrome per rilevare errori
   Argomenti: m (bit parità), parola ricevuta
   Casi base: quando lista [0..m-1] è vuota sindrome = 0
   Casi generali: per ogni bit di parità verifica correttezza e accumula errori
   usa foldl' per verificare ogni bit di parità e costruire sindrome -}
calcolaSindrome :: Int -> ParolaBinaria -> Int
calcolaSindrome m parola =
    let n = length parola
    in foldl' (\sindrome i -> 
            let posizioneParita = 2^i
                indiceParita = posizioneParita - 1
                indiciControllati = [ j-1 | j <- [1..n], testBit j i ]
                bitDati = map (parola !!) (filter (/= indiceParita) indiciControllati)
                valoreAtteso = foldl' xor 0 bitDati
                valoreParita = parola !! indiceParita
                {- Se la parità non corrisponde, aggiungi la posizione alla sindrome -}
            in if valoreParita /= valoreAtteso 
                then sindrome + posizioneParita 
                else sindrome
        ) 0 [0..m-1]

{- estraiDati: Estrae i bit di dati da una parola codificata
   Argomenti: m (bit parità), parola codificata
   filtra posizioni non-parità e mappa gli elementi -}
estraiDati :: Int -> ParolaBinaria -> ParolaBinaria
estraiDati m parola =
    let n = length parola
        indiciDati = filter (\i -> not (potenzaDiDue (i+1))) [0..n-1]
    in map (parola !!) indiciDati

{- decodificaHamming: Decodifica una parola e corregge un eventuale errore
   Argomenti: m (bit parità), parola ricevuta
   calcola sindrome e decide se correggere o meno -}
decodificaHamming :: Int -> ParolaBinaria -> (ParolaBinaria, Maybe Int)
decodificaHamming m parola =
    let sindrome = calcolaSindrome m parola
    in if sindrome == 0
        then (estraiDati m parola, Nothing)
        else
            let posizioneErrore = sindrome - 1
                parolaCorretta = aggiornaElemento parola posizioneErrore (1 - parola !! posizioneErrore)
            in (estraiDati m parolaCorretta, Just (posizioneErrore + 1))

{- calcolaDistanzaHamming: Calcola la distanza di Hamming tra due parole
   Argomenti: parola1, parola2 -}
calcolaDistanzaHamming :: ParolaBinaria -> ParolaBinaria -> Int
calcolaDistanzaHamming parola1 parola2 = 
    sum (zipWith (\a b -> if a == b then 0 else 1) parola1 parola2)
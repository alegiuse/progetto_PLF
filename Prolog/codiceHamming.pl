/* #######################################################
# Corso di Programmazione Logica e Funzionale            #
# Progetto per la sessione autunnale A.A. 2024/2025      #
# di Nasrine Aboufaris, Matricola: 321885                #
# e Alessia Giuseppetti, Matricola: 322984               #
# Anno di corso: Terzo                                   #
##########################################################
Specifica: Programma Prolog per la codifica/decodifica   #
di codici di Hamming e calcolo della distanza di Hamming #*/

/* Predicato principale del programma. Fornisce istruzioni all'utente per l'uso del programma. */
main :- write('------------------------Benvenuto------------------------'), nl,
        write('Il seguente programma permette di utilizzare i codici di Hamming generici (n,k) per '), nl,
        write('la codifica, decodifica e calcolo della distanza di Hamming tra due parole binarie. '), nl,
        write('Si ricorda che la decodifica di Hamming di seguito implementata permette solo di '), nl,
        write('Individuare e correggere un solo errore. '), nl,
        write('Come funziona il programma: '), nl,
        write('  1. Ti verra\' richiesto il numero di bit di parita\' m.'), nl,
        write('  2. Da m verranno calcolati automaticamente n e k.'), nl,
        write('  3. Segui le istruzioni del menu per completare l\'operazione desiderata. '), nl,
        write('La parola deve essere inserita come una lista di interi,'), nl,
        write('si prega di usare la seguente formattazione: '), nl,
        write('Scrivere la parola tra parentesi quadre e inserire una virgola tra un numero e l\'altro.'), nl,
        write('Esempio di parola accettata: [1, 0, 1, 0].'), nl,
        write('Esempio di parola non accettata 1010.'), nl,
        write('Si prega di inserire il punto finale prima dell\'invio e si raccomanda di seguire questa formattazione'), nl,
        write('per evitare anomalie durante l\'esecuzione del programma.'), nl,
        write('----------------------------------------------------------'), nl,
        menu.

/* Il predicato menu mostra le opzioni disponibili e 
   acquisice tramite tastiera la scelta dell'utente. */
menu :- write('Scegliere l\'operazione che si desidera eseguire: '), nl,
        write(' 1. Codifica di Hamming'), nl,
        write(' 2. Decodifica di Hamming'), nl,
        write(' 3. Distanza di Hamming'), nl,
        write(' 4. Esci'), nl,
        read(Opzione),
        esegui_opzione(Opzione).

/* Il predicato esegui_opzione(1) esegue la codifica di Hamming. Chiede il numero di bit di parità M,
   calcola il parametro k, valida la parola input P di lunghezza k ed esegue la codifica. 
     - Il primo argomento è l'opzione 1 della scelta dell'utente. */
esegui_opzione(1) :- parita(M),    
                     parametri_hamming(M, K, _N),
                     acquisisci_parola(Parola, K),
                     write('Esecuzione codifica...'), nl,
                     codifica_parola(Parola, M),
                     menu. 

/* Il predicato esegui_opzione(2) esegue la Decodifica di Hamming. Chiede il numero di bit di parità M, 
   calcola il parametro n, valida il codice ricevuto di lunghezza n ed esegue la decodifica. 
    - Il primo argomento è l'opzione 2 della scelta dell'utente. */
esegui_opzione(2) :- parita(M),    
                     parametri_hamming(M, _K, N),
                     acquisisci_parola(Parola, N),
                     write('Esecuzione decodifica...'), nl,
                     decodifica_parola(Parola, M),
                     menu. 

/* Il predicato esegui_opzione(3) calcola la distanza di Hamming tra due parole.
   Acquisisce due parole binarie, ne verifica la validità, controlla che abbiano la stessa lunghezza
   e calcola la distanza.
    - Il primo argomento è l'opzione 3 scelta dall'utente. */
esegui_opzione(3) :- write('Inserire la prima parola: '), nl,
                     acquisisci_parola_distanza(T1),        
                     write('Inserire la seconda parola: '), nl, 
                     acquisisci_parola_distanza(T2),            
                     confronto_lunghezza(T1, T2),                
                     calcolo_distanza(T1, T2),               
                     menu.

/* Il predicato esegui_opzione(4) si occupa di terminare il programma. 
   Termina l'esecuzione del programma dopo aver stampato un messaggio di saluto.
   - Il primo argomento è l'opzione 4 scelta dall'utente. */
esegui_opzione(4) :- write('Grazie per aver utilizzato il programma. Arrivederci!'), nl,
                     halt.  % Termina il programma

/* Il predicato esegui_opzione(_) gestisce opzioni non valide. 
   Visualizza un messaggio di errore e torna al menu principale.
   - Il primo argomento è un'opzione non compresa tra 1 e 4. */
esegui_opzione(_) :- write('Opzione non valida. Riprovare.'), nl,
                     menu.  

/* Il predicato parita(M) acquisisce da tastiera il numero di bit di parità scelto 
   dall'utente e ne verifica la validità, assicurandosi che il numero 
   sia maggiore o uguale a 2. */
parita(M) :- nl, write('Il bit di parita\' deve essere maggiore o uguale a 2. '), nl,
             write('Inserire il bit di parita\' scelto:'), nl,
             % lettura dell'input
             (catch(read(M), _, fail),
             % validazione dell'input
             integer(M), 
             M >= 2
             -> 
                true
             ; 
                 nl, write('Input non valido! Deve essere un intero >= 2.'), nl,
                 parita(M)
             ).

/* Il predicato parametri_hamming(M, K, N) calcola i parametri k(bit di dati) ed n(bit totali)
   per il codice di Hamming.
    - Il primo argomento rappresenta il numero di bit di parità.
    - Il secondo argomento rappresenta il numero di bit di dati.
    - Il terzo argomento rappresenta il numero totale di bit. 
   Esempio: parametri_hamming(3, K, N) calcola K=4 e N=7. */
parametri_hamming(M,K,N):- K is 2^M - 1 - M,
                           N is 2^M -1.

/* Il predicato acquisisci_parola acquisisce una parola binaria dall'input
   e la valida rispetto alla lunghezza specificata. 
   - Il primo argomento è la parola validata in output.
   - Il secondo argomento è la lunghezza attesa della parola. */
acquisisci_parola(Parola, Lunghezza) :- write('Inserire la parola di lunghezza '),
                                        write(Lunghezza),
                                        write(': '),
                                        (catch(read(Input), _, fail)
                                        ->  processa_input(Input, Lunghezza, Parola)
                                        ;   nl, write('Errore di acquisizione!'), nl, 
                                            acquisisci_parola(Parola, Lunghezza)
                                        ).

/* Il predicato processa_input gestisce la validazione dell'input e la ricorsione
   in caso di errore. Se l'input è valido, unifica Parola con Input; altrimenti,
   stampa un messaggio di errore e richiede nuovo input.
   - Il primo argomento è l'input letto da tastiera.
   - Il secondo argomento è la lunghezza attesa.
   - Il terzo argomento è l'output validato. */
processa_input(Input, Lunghezza, Parola) :- (valida_parola_binaria(Input, Lunghezza)
                                            ->  Parola = Input
                                            ;   write('Parola non valida. Deve essere lunga '),
                                                write(Lunghezza),
                                                write(' e contenere solo 0 e 1.'), nl,
                                                acquisisci_parola(Parola, Lunghezza)
                                            ).



/* Il predicato valida_parola_binaria verifica che una lista sia valida per il codice di Hamming, controllando che sia
   una lista, che abbia che abbia lunghezza Lunghezza e che tutti gli elementi siano bit validi.
   - il primo argomento è la lista da verificare
   - il secondo argomento è la lunghezza della parola attesa. */
valida_parola_binaria(Parola, Lunghezza) :- is_list(Parola),
                                            length(Parola, Lunghezza),
                                            maplist(bit_valido, Parola).

/* Il fatto bit_valido definisce i bit validi per il codice di Hamming.
   Un bit è valido se è 0 o 1.
   - L'unico argomento rappresenta il bit da verificare. */
bit_valido(0).
bit_valido(1).

/* Il predicato acquisisci_parola_distanza acquisisce una parola binaria generica dall'input
   e la valida. 
   - L'unico argomento è la parola validata in output. */
acquisisci_parola_distanza(Parola) :- (catch(read(Input), _, fail), 
                                       processa_input_distanza(Input, Parola)
                                      ; nl, write('Errore di acquisizione!'), nl, 
                                        acquisisci_parola_distanza(Parola)
                                      ).

/* Il predicato processa_input_distanza gestisce la validazione dell'input e la ricorsione
   in caso di errore. Se l'input è valido, unifica Parola con Input; altrimenti,
   stampa un messaggio di errore e richiede nuovo input.
   - Il primo argomento Input è l'input letto da tastiera.
   - Il secondo argomento Parola è l'output validado. */
processa_input_distanza(Input, Parola) :- (validazione_parola_distanza(Input),
                                           Parola = Input
                                          ; write('Parola non valida. Deve essere una lista contenente solo 0 e 1.'), nl,
                                            acquisisci_parola_distanza(Parola)
                                          ).

/* Il predicato validazione_parola_generica verifica che l'input sia una lista 
   composta esclusivamente da valori binari (0 e 1). 
   - L'unico argomento rappresenta la parola da validare. */
validazione_parola_distanza(Parola) :- is_list(Parola),
                                       maplist(bit_valido, Parola).

/* Il predicato confronto_lunghezza verifica che due liste abbiano la stessa lunghezza,
   condizione necessaria per il calcolo della distanza di Hamming.
   - Il primo argomento rappresenta la prima lista da confrontare.
   - Il secondo argomento rappresenta la seconda lista da confrontare. */
confronto_lunghezza(T1, T2) :- length(T1, L1),
                               length(T2, L2),
                               (L1 =:= L2
                               -> true
                               ;  write('Le due parole devono avere la stessa lunghezza.'), nl,
                                  write('Prima parola lunghezza: '), write(L1), nl,
                                  write('Seconda parola lunghezza: '), write(L2), nl,
                                  write('Inserire nuovamente le due parole.'), nl,
                                  esegui_opzione(3)
                                ).

/* Il predicato posizione_parita calcola, dato un indice, se la posizione è potenza di due o meno.
    - L'unico argomento indica la posizione che gli viene passata. */
posizione_parita(Posizione) :- Posizione > 0,
                               (Posizione /\ (Posizione - 1)) =:= 0.
        
/* Il predicato inserisci_bit inserisce bit di dati nelle posizioni non di parità di un codice Hamming, 
   lasciando dei segnaposti per i bit di parità. 
    - Il primo argomento rappresenta la posizione corrente nel codice.
    - Il secondo argomento è la lista di bit di dati da inserire.
    - Il terzo argomento è il codice risultante con i bit di dati nelle posizioni appropriate. */
inserisci_bit(_, [], []) :- !.
inserisci_bit(Pos, [B|Coda], [B|Resto]) :- \+ posizione_parita(Pos),
                                           PosSucc is Pos + 1,
                                           inserisci_bit(PosSucc, Coda, Resto).
inserisci_bit(Pos, BitDati, [_|Resto]) :- posizione_parita(Pos),
                                          PosSucc is Pos + 1,
                                          inserisci_bit(PosSucc, BitDati, Resto).      

/* Il predicato calcolo_parita calcola il valore dei bit di parità per una specifica posizione P
   in una lista di bit, utilizzando l'operazione XOR tra i bit nelle posizioni appropriate.
    - Il primo argomento è la lista di bit su cui operare.
    - Il secondo argomento indica la posizione del bit di parità da calcolare.
    - Il terzo argomento è il valore calcolato del bit di parità. */
calcolo_parita(Lista, Pos, Valore_P) :- calcolo_parita(Lista, Pos, 1, 0, Valore_P).

/* Il predicato calcolo_parita implementa il calcolo ricorsivo del bit di parità.
   - Il primo argomento è la lista di bit rimanente da processare.
   - Il secondo argomento è la posizione del bit di parità.
   - Il terzo argomento è la posizione corrente nella lista.
   - Il quarto argomento è l'accumulatore per il calcolo XOR.
   - Il quinto argomento è il valore finale del bit di parità. */
calcolo_parita([], _, _, Acc, Acc).
calcolo_parita([Bit|Resto], Pos, Indice, Acc, Valore_P) :- ((Indice /\ Pos) > 0 ->
                                                               xor_bit(Acc, Bit, NuovoAcc)
                                                           ;   NuovoAcc = Acc
                                                           ),
                                                           ProssimoIndice is Indice + 1,
                                                           calcolo_parita(Resto, Pos, ProssimoIndice, NuovoAcc, Valore_P).

/* Fatto xor_bit implementa l'operazione XOR tra due bit. 
    - Il primo argomento è il primo bit.
    - Il secondo argomento è il secondo bit.
    - Il terzo argomento è il risultato dell'operazione XOR. */
xor_bit(0, 0, 0).
xor_bit(0, 1, 1).
xor_bit(1, 0, 1).
xor_bit(1, 1, 0).


/* Il predicato sostituisci_elemento sostituisce l'elemento in una specifica posizione di una lista.
   - Il primo argomento è la lista originale.
   - Il secondo argomento è la posizione dell'elemento da sostituire.
   - Il terzo argomento è il nuovo valore da inserire.
   - Il quarto argomento è la lista risultante dopo la sostituzione. */
sostituisci_elemento([_|T], 1, X, [X|T]).
sostituisci_elemento([H|T], Pos, X, [H|T1]) :- Pos > 1,
                                               Pos1 is Pos - 1,
                                               sostituisci_elemento(T, Pos1, X, T1).

/* Il predicato calcola_tutte_parita calcola tutti i bit di parità per una lista di posizioni
   e li inserisce nel codice.
   - Il primo argomento è il codice temporaneo senza bit di parità.
   - Il secondo argomento è la lista delle posizioni dei bit di parità.
   - Il terzo argomento è il codice completo con tutti i bit di parità calcolati. */

calcola_tutte_parita(List, [], List).
calcola_tutte_parita(List, [P|Resto], Risultato) :- calcolo_parita(List, P, PV),
                                                    sostituisci_elemento(List, P, PV, NuovaLista),
                                                    calcola_tutte_parita(NuovaLista, Resto, Risultato).

/* Il predicato parita_list genera la lista delle posizioni dei bit di parità per un dato numero M.
   Le posizioni sono le potenze di 2 fino a 2^(M-1).
   - Il primo argomento è il numero di bit di parità.
   - Il secondo argomento è la lista delle posizioni dei bit di parità. */
parita_list(M, List) :- M1 is M - 1,
                        findall(P, (between(0, M1, Exp), P is 2^Exp), List).

/* Il predicato codifica_parola implementa la codifica di Hamming per una parola di dati.
   - Il primo argomento è la parola di dati da codificare.
   - Il secondo argomento è il numero di bit di parità.
   Il predicato inserisce i bit di dati nelle posizioni appropriate,
   calcola i bit di parità e produce il codice di Hamming finale. */
codifica_parola(Parola, M) :-  % Calcola le posizioni dei bit di parità
                               parita_list(M, PosizioniParita),
                               % Inserisce i bit di dati nelle posizioni appropriate
                               inserisci_bit(1, Parola, CodiceTemp),
                               % Calcola i valori dei bit di parità e li inserisce
                               calcola_tutte_parita(CodiceTemp, PosizioniParita, CodiceHamming),
                               write('Codice Hamming: '),
                               write(CodiceHamming), nl.



/* Il predicato calcola_sindrome calcola la sindrome per un codice Hamming ricevuto.
   La sindrome rappresenta la somma delle posizioni dei bit di parità che risultano errati.
   Se la sindrome è 0, non ci sono errori, in caso contrario indica la posizione dell'errore.
    - Il primo argomento è il codice di Hamming da analizzare.
    - Il secondo argomento è la lista delle posizioni dei bit di parità.
    - Il terzo argomento è il valore calcolato della sindrome. */
calcola_sindrome(_, [], 0).
calcola_sindrome(Parola, [P|Resto], Sindrome) :- calcolo_parita(Parola, P, Valore_P),
                                                 calcola_sindrome(Parola, Resto, SindromeParziale),
                                                 (Valore_P =:= 1 ->
                                                  Sindrome is SindromeParziale + P
                                                 ; Sindrome = SindromeParziale).


/* Il predicato elemento_in restituisce l'elemento in una specifica posizione di una lista
   utilizzando la numerazione basata su 1 (il primo elemento è in posizione 1).
   - Il primo argomento è la lista in cui cercare.
   - Il secondo argomento è la posizione dell'elemento da estrarre.
   - Il terzo argomento è l'elemento trovato nella posizione specificata. */
elemento_in([X|_], 1, X).
elemento_in([_|T], Pos, X) :- Pos > 1,
                              Pos1 is Pos - 1,
                              elemento_in(T, Pos1, X).

/* Il predicato correggi_errore corregge un singolo errore bit in un codice di Hamming
   invertendo il valore del bit nella posizione specificata (0 diventa 1, 1 diventa 0).
   - Il primo argomento è il codice originale contenente l'errore.
   - Il segundo argomento è la posizione del bit errato.
   - Il terzo argomento è il codice con l'errore corretto. */
correggi_errore(Lista, Pos, ListaCorretta) :- elemento_in(Lista, Pos, Bit),
                                              (Bit =:= 0 -> NuovoBit = 1 ; NuovoBit = 0),
                                              sostituisci_elemento(Lista, Pos, NuovoBit, ListaCorretta).

/* Il predicato estrai_dati estrae i bit di dati da un codice di Hamming,
   rimuovendo i bit di parità che si trovano nelle posizioni che sono potenze di 2.
   - Il primo argomento è il codice Hamming completo.
   - Il secondo argomento è la lista contenente solo i bit di dati. */
estrai_dati(Codice, Dati) :- estrazione_dati(Codice, 1, Dati).

/* Predicato ricorsivo per estrai_dati che effettua l'estrazione effettiva
   dei bit di dati, saltando i bit di parità nelle posizioni che sono potenze di 2.
   - Il primo argomento è la porzione rimanente del codice da processare.
   - Il secondo argomento è la posizione corrente nel codice (contatore).
   - Il terzo argomento è la lista accumulata dei bit di dati estratti. */
estrazione_dati([], _, []).
estrazione_dati([Bit|Resto], Pos, [Bit|DatiResto]) :- \+ posizione_parita(Pos),
                                                      !,
                                                      PosSucc is Pos + 1,
                                                      estrazione_dati(Resto, PosSucc, DatiResto).
estrazione_dati([_|Resto], Pos, Dati) :- posizione_parita(Pos),
                                         !,
                                         PosSucc is Pos + 1,
estrazione_dati(Resto, PosSucc, Dati).

/* Il predicato decodifica_parola implementa la decodifica completa di un codice di Hamming.
   Calcola la sindrome del codice ricevuto: se è zero non ci sono errori, altrimenti 
   corregge l'errore nella posizione indicata e estrae i bit di dati.
   - Il primo argomento CodiceRicevuto è il codice Hamming da decodificare.
   - Il secondo argomento M è il numero di bit di parità utilizzati. */
decodifica_parola(CodiceRicevuto, M) :- parita_list(M, PosizioniParita),
                                        calcola_sindrome(CodiceRicevuto, PosizioniParita, Sindrome),
                                        (Sindrome =:= 0 ->
                                         write('Nessun errore trovato.'), nl,
                                         estrai_dati(CodiceRicevuto, Dati)
                                        ; write('Errore trovato in posizione: '), write(Sindrome), nl,
                                          correggi_errore(CodiceRicevuto, Sindrome, CodiceCorretto),
                                          estrai_dati(CodiceCorretto, Dati)
                                        ),
                                        write('Parola decodificata: '), write(Dati), nl.

/* Predicato che esegue il calcolo di Hamming in maniera ricorsiva. 
    - il primo argomento indica la prima lista inserita;
    - il secondo argomento indica la seconda lista inserita;
    - il terzo argomento è la distanza di Hamming risultante.
    Il calcolo della distanza è stato fatto in maniera ricorsiva:*/

calcolo_distanza([], [], 0).
calcolo_distanza([H1|T1], [H2|T2], D) :- calcolo_distanza(T1, T2, D1),
                                         (  H1 =:= H2
                                         ->  D = D1
                                         ;  D is D1 + 1
                                         ).
calcolo_distanza(T1, T2) :- calcolo_distanza(T1, T2, D),
                            write('Distanza di Hamming: '), write(D), nl.
import os
import re
import difflib
from pyswip import Prolog

def check_parsability(file_path):
    """
    Verifica se un file Prolog è sintatticamente corretto.
    Restituisce un punteggio (1.0 per successo, 0.0 per fallimento) e un messaggio.
    """
    prolog = Prolog()
    try:
        prolog.consult(file_path)
        return 1.0, "Successo: Il file è sintatticamente corretto."
    except Exception as e:
        return 0.0, f"Errore di Parsing: Il file non è valido. Dettagli: {e}"

def normalize_prolog_text(text_content):
    """
    Normalizza una stringa di testo Prolog per un confronto coerente.
    """
    # Rimuovi i commenti multi-riga (/* ... */)
    content = re.sub(r'/\*.*?\*/', '', text_content, flags=re.DOTALL)
    
    lines = content.splitlines()
    normalized_lines = []
    
    for line in lines:
        # Rimuovi i commenti a riga singola (%)
        line_no_comment = re.sub(r'%.*$', '', line)
        # Rimuovi spaziature extra e uniforma la terminazione dei fatti
        stripped_line = line_no_comment.strip().replace(" ", "")
        if stripped_line:
            normalized_lines.append(stripped_line)
            
    # Ordina le righe per un confronto indipendente dall'ordine
    normalized_lines.sort()
    return "\n".join(normalized_lines)

def calculate_semantic_accuracy_ratio(generated_kb_path, gold_standard_kb_path):
    """
    Calcola un rapporto di similarità (0-1) tra due file Prolog.
    Il confronto ignora commenti, spaziature e ordine dei fatti.
    """
    try:
        with open(generated_kb_path, 'r', encoding='utf-8') as f:
            generated_content = f.read()
        with open(gold_standard_kb_path, 'r', encoding='utf-8') as f:
            gold_content = f.read()
    except FileNotFoundError:
        return 0.0, "Errore: Uno o entrambi i file non sono stati trovati."

    normalized_gen = normalize_prolog_text(generated_content)
    normalized_gold = normalize_prolog_text(gold_content)

    # Usa SequenceMatcher per ottenere un rapporto di similarità
    matcher = difflib.SequenceMatcher(None, normalized_gold, normalized_gen)
    ratio = matcher.ratio()
    
    message = f"Successo: Il rapporto di similarità è {ratio:.4f}."
    if ratio < 1.0:
        message = f"Discrepanza: Il rapporto di similarità è {ratio:.4f}."
        
    return ratio, message

def check_solvability(kb_path, goal_query):
    """
    Verifica se un obiettivo è risolvibile.
    Restituisce un punteggio (1.0 per successo, 0.0 per fallimento) e un messaggio.
    """
    prolog = Prolog()
    try:
        prolog.consult(kb_path)
        solutions = list(prolog.query(goal_query))

        if solutions:
            return 1.0, f"Successo: L'obiettivo '{goal_query}' è stato risolto."
        else:
            return 0.0, f"Fallimento: Nessuna soluzione trovata per l'obiettivo '{goal_query}'."
    except Exception as e:
        return 0.0, f"Errore durante il tentativo di risoluzione: {e}"

def evaluate_knowledge_base(generated_file, gold_standard_file, goal_to_solve):
    """
    Esegue la valutazione completa di un file KB e restituisce un dizionario con i punteggi.
    """
    results = {}

    # 1. Valutazione della Parsabilità
    parsability_score, parsability_msg = check_parsability(generated_file)
    results['parsability'] = {'score': parsability_score, 'message': parsability_msg}

    # Se non è parsabile, le altre metriche sono automaticamente 0
    if parsability_score == 0.0:
        results['semantic_accuracy'] = {'score': 0.0, 'message': 'Non valutabile: il file non è parsabile.'}
        results['solvability'] = {'score': 0.0, 'message': 'Non valutabile: il file non è parsabile.'}
        return results

    # 2. Valutazione dell'Accuratezza Semantica
    accuracy_score, accuracy_msg = calculate_semantic_accuracy_ratio(generated_file, gold_standard_file)
    results['semantic_accuracy'] = {'score': accuracy_score, 'message': accuracy_msg}

    # 3. Valutazione della Risolvibilità
    solvability_score, solvability_msg = check_solvability(generated_file, goal_to_solve)
    results['solvability'] = {'score': solvability_score, 'message': solvability_msg}
    
    return results

def main():
    """
    Funzione principale per eseguire il processo di valutazione e stampare i risultati.
    """
    # --- CONFIGURAZIONE ---
    generated_file = "generated_kb_imperfect.txt"
    gold_standard_file = "gold_standard_kb.txt"
    goal_to_solve = "can_move(robot, room_a, room_b)"

    print(f"--- Inizio valutazione del file: {generated_file} ---")

    # Esegui la valutazione
    evaluation_results = evaluate_knowledge_base(generated_file, gold_standard_file, goal_to_solve)

    # Stampa i risultati in modo formattato
    for metric, result in evaluation_results.items():
        print(f"\n[ METRICA: {metric.replace('_', ' ').upper()} ]")
        print(f"  - Punteggio: {result['score']:.4f}")
        print(f"  - Dettaglio: {result['message']}")
    
    # Calcola e stampa un punteggio medio
    average_score = sum(result['score'] for result in evaluation_results.values()) / len(evaluation_results)
    print("\n-----------------------------------------")
    print(f"PUNTEGGIO MEDIO COMPLESSIVO: {average_score:.4f}")
    print("-----------------------------------------")


if __name__ == "__main__":
    
    # 1. Gold Standard
    with open("gold_standard_kb.txt", "w", encoding='utf-8') as f:
        f.write("% Gold standard KB\n")
        f.write("location(robot, room_a).\n")
        f.write("object(key).\n")
        f.write("connected(room_a, room_b).\n")
        f.write("can_move(Agent, From, To) :- location(Agent, From), connected(From, To).\n")

    # 2. Generato
    with open("generated_kb_imperfect.txt", "w", encoding='utf-8') as f:
        f.write("/* KB generata dal modello */\n\n")
        f.write("can_move(Agent, From, To) :- location(Agent, From), conected(From, To).\n") # ERRORE QUI
        f.write("object(key).\n")
        f.write("location(robot, room_a).\n")
        
    main()
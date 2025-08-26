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

def calculate_semantic_accuracy_ratio(generated_kb_path, gold_standard_kb_path):
    try:
        with open(generated_kb_path, 'r', encoding='utf-8') as f:
            generated_content = f.read()
        with open(gold_standard_kb_path, 'r', encoding='utf-8') as f:
            gold_content = f.read()
    except FileNotFoundError:
        return 0.0, "Errore: Uno o entrambi i file non sono stati trovati."

    generated_facts = extract_normalized_facts_as_set(generated_content)
    gold_facts = extract_normalized_facts_as_set(gold_content)

    if not gold_facts:
        return 1.0, "Successo: Il file gold standard è vuoto o contiene solo commenti."

    matching_facts_count = len(gold_facts & generated_facts)
    
    total_gold_facts = len(gold_facts)
    accuracy = matching_facts_count / total_gold_facts
    
    message = f"Success: Trovati {matching_facts_count} su {total_gold_facts} fatti richiesti. Accuratezza: {accuracy:.2%}."
    if accuracy < 1.0:
        message = f"Discrepanza: Trovati {matching_facts_count} su {total_gold_facts} fatti richiesti. Accuratezza: {accuracy:.2%}."
        
    return accuracy, message

def extract_normalized_facts_as_set(text_content):
    content = re.sub(r'/\*.*?\*/', '', text_content, flags=re.DOTALL)
    
    lines = content.splitlines()
    normalized_facts = set()
    
    for line in lines:
        line_no_comment = re.sub(r'%.*$', '', line)
        stripped_line = re.sub(r'\s+', '', line_no_comment)
        
        if stripped_line:
            normalized_facts.add(stripped_line)
            
    return normalized_facts

def evaluate_knowledge_base(generated_file, gold_standard_file):
    results = {}

    # 1. Parsability
    parsability_score, parsability_msg = check_parsability(generated_file)
    results['parsability'] = {'score': parsability_score, 'message': parsability_msg}

    # 2.Accuracy
    accuracy_score, accuracy_msg = calculate_semantic_accuracy_ratio(generated_file, gold_standard_file)
    results['semantic_accuracy'] = {'score': accuracy_score, 'message': accuracy_msg}

    # Parsability check for Solvability
    if parsability_score == 0.0:
        results['solvability'] = {'score': 0.0, 'message': 'File non parsabile.'}
        return results

    # 3. Solvability
    # NOTE: it's not easy to define if it's solvable or not, because it depends on the planner to, so we make it manually checking the code.
    # Here we pass a value of 1 if it's parsable and if the accuracy is more than 75%
    if accuracy_score > 0.75:
        solvability_score = 1.0
        solvability_msg = "Solvibile: accuratezza semantica > 75%."
    else:
        solvability_score = 0.0
        solvability_msg = f"Non solvibile: accuratezza ({accuracy_score:.2%}) inferiore a 75%."
    results['solvability'] = {'score': solvability_score, 'message': solvability_msg}
    
    return results

def main():
    FILE_PATH = os.path.join(os.path.dirname(__file__), '..', 'exps', 'multi-steps', 'blocks_world', '10')
    # --- CONFIGURAZIONE ---
    generated_file = os.path.join(FILE_PATH, 'qwen3_qlora_output.txt')
    gold_standard_file = os.path.join(FILE_PATH, 'output.txt')

    print(f"--- Inizio valutazione del file: {generated_file} ---")

    # Esegui la valutazione
    evaluation_results = evaluate_knowledge_base(generated_file, gold_standard_file)

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
    main()
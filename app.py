from flask import Flask, request, jsonify
import subprocess

app = Flask(__name__)

@app.route('/executar', methods=['POST'])
def executar_cobol():
    dados = request.json

    # Escreve os dados de entrada num arquivo, se necessário
    with open("entrada.txt", "w") as f:
        f.write(dados.get("entrada", ""))

    # Compila o COBOL
    resultado_compilacao = subprocess.run(["cobc", "-x", "meu_programa.cob"], capture_output=True, text=True)

    if resultado_compilacao.returncode != 0:
        return jsonify({"erro": "Erro na compilação", "mensagem": resultado_compilacao.stderr}), 500

    # Executa o programa COBOL compilado
    resultado_execucao = subprocess.run(["./meu_programa"], capture_output=True, text=True)

    return jsonify({
        "saida": resultado_execucao.stdout,
        "erro": resultado_execucao.stderr
    })

if __name__ == '__main__':
    app.run(debug=True)

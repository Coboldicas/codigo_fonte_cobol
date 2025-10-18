import pytchat
from collections import deque

# COLOQUE AQUI O ID DO VÍDEO DA SUA LIVE
video_id = "ABC123XYZ"

# Mantém só as últimas 5 mensagens
ultimas_msgs = deque(maxlen=5)

chat = pytchat.create(video_id=video_id)

while chat.is_alive():
    for c in chat.get().sync_items():
        # Adiciona no buffer
        ultimas_msgs.append(f"{c.author.name}: {c.message}")

        # Escreve no arquivo TXT
        with open("/home/bopnet/obs_chat.txt", "w", encoding="utf-8") as f:
            f.write("\n".join(ultimas_msgs))

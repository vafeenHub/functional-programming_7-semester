# Используем официальный образ — GHC уже установлен!
FROM haskell:9.6

# Устанавливаем curl для ghcup
RUN apt-get update && \
    apt-get install -y curl ca-certificates gnupg && \
    rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    

WORKDIR /workspace
CMD ["bash"]
# Relatório de Telemetria de Execução do Exemplo Real

Este relatório foi gerado automaticamente pelo console de demonstração do **RESTRequest4Delphi** 
em **30/06/2026 14:37:42**.

## 📊 Resumo das Estatísticas

- **Total de Requisições**: 9
- **Taxa de Sucesso**: 9/9 (100,0%)
- **Tempo Total Gasto**: 1624 ms
- **Tempo Médio por Requisição**: 180,44 ms
- **Maior Latência Registrada**: 844 ms

## 📋 Detalhamento das Transações

| Passo / Descrição | Verbo | URL Alvo | Status HTTP | Latência (ms) | Bytes Recebidos | Resultado |
| :--- | :---: | :--- | :---: | :---: | :---: | :--- |
| Listar Posts (UserId=1) | GET | https://jsonplaceholder.typicode.com/posts?userId=1 | 200 | 187 | 2726 | 🟢 Sucesso |
| Obter Post por ID (URL Segment) | GET | https://jsonplaceholder.typicode.com/posts/1 | 200 | 32 | 292 | 🟢 Sucesso |
| Criar Novo Post (JSON Body) | POST | https://jsonplaceholder.typicode.com/posts | 201 | 140 | 151 | 🟢 Sucesso |
| Atualizar Post Completo (PUT) | PUT | https://jsonplaceholder.typicode.com/posts/1 | 200 | 125 | 105 | 🟢 Sucesso |
| Deletar Post (DELETE) | DELETE | https://jsonplaceholder.typicode.com/posts/1 | 200 | 140 | 2 | 🟢 Sucesso |
| Gravar Cookies (AddCookie) | GET | https://httpbin.org/cookies/set?session_token=r4d_token_123 | 200 | 844 | 60 | 🟢 Sucesso |
| Upload Multipart (AddField) | POST | https://httpbin.org/post | 200 | 156 | 676 | 🟢 Sucesso |
| Chamada Assíncrona Paralela 1 | GET | https://httpbin.org/delay/1 | 200 | 0 | 411 | 🟢 Sucesso |
| Chamada Assíncrona Paralela 2 | GET | https://httpbin.org/delay/1 | 200 | 0 | 413 | 🟢 Sucesso |

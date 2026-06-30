# RelatÃ³rio de Benchmark Comparativo de Performance: Engines HTTP

RelatÃ³rio tÃ©cnico consolidando o desempenho e conformidade de requisiÃ§Ãµes das engines nativas do Delphi (**Delphi Client**, **NetHTTPClient** e **Indy**) contra a API pÃºblica real **https://httpbin.org**.

* **Data/Hora**: 2026-06-30 15:10:38
* **IteraÃ§Ãµes por MÃ©todo**: 5 iteraÃ§Ãµes reais (totalizando 25 requisiÃ§Ãµes por engine)
* **Ambiente**: Windows 10/11 x64, Delphi DCC32 Compiler v35.0

---

## ðŸ“Š Tabela de Performance Comparativa (LatÃªncia MÃ©dia em ms)

A tabela abaixo exibe o tempo mÃ©dio de resposta de cada mÃ©todo HTTP. Valores menores (mais rÃ¡pidos) indicam melhor desempenho.

| MÃ©todo HTTP | Delphi Client (Nativo) | NetHTTPClient | Indy Engine | Vencedor (Mais RÃ¡pida) |
| :--- | :---: | :---: | :---: | :---: |
| **GET** | 147 ms | 170 ms | 629 ms | **Nativo** ðŸš€ |
| **POST** | 148 ms | 180 ms | 749 ms | **Nativo** ðŸš€ |
| **PUT** | 166 ms | 167 ms | 879 ms | **Nativo** ðŸš€ |
| **DELETE** | 170 ms | 140 ms | 609 ms | **NetHTTP** ðŸš€ |
| **PATCH** | 4.140 ms | 383 ms | 811 ms | **NetHTTP** ðŸš€ |

---

## ðŸ“ˆ AnÃ¡lise Detalhada de Conformidade (Taxa de Sucesso)

| Engine | MÃ©todo | RequisiÃ§Ãµes | Sucessos | Taxa de Sucesso | Status de Conformidade |
| :--- | :---: | :---: | :---: | :---: | :---: |
| **Nativo** | GET | 5 | 5 | 100% | âœ… 100% Conforme |
| **Nativo** | POST | 5 | 5 | 100% | âœ… 100% Conforme |
| **Nativo** | PUT | 5 | 5 | 100% | âœ… 100% Conforme |
| **Nativo** | DELETE | 5 | 5 | 100% | âœ… 100% Conforme |
| **Nativo** | PATCH | 5 | 5 | 100% | âœ… 100% Conforme |
| **NetHTTP** | GET | 5 | 5 | 100% | âœ… 100% Conforme |
| **NetHTTP** | POST | 5 | 5 | 100% | âœ… 100% Conforme |
| **NetHTTP** | PUT | 5 | 5 | 100% | âœ… 100% Conforme |
| **NetHTTP** | DELETE | 5 | 5 | 100% | âœ… 100% Conforme |
| **NetHTTP** | PATCH | 5 | 5 | 100% | âœ… 100% Conforme |
| **Indy** | GET | 5 | 5 | 100% | âœ… 100% Conforme |
| **Indy** | POST | 5 | 5 | 100% | âœ… 100% Conforme |
| **Indy** | PUT | 5 | 5 | 100% | âœ… 100% Conforme |
| **Indy** | DELETE | 5 | 5 | 100% | âœ… 100% Conforme |
| **Indy** | PATCH | 5 | 5 | 100% | âœ… 100% Conforme |

---

## ðŸ” ConclusÃµes Arquiteturais

1. **EficiÃªncia no Acesso Real**: Todas as engines ativas obtiveram **100% de taxa de sucesso** (todas retornaram HTTP 200 OK), o que prova a conformidade funcional completa da biblioteca contra APIs reais publicadas na web.
2. **LatÃªncia de ConexÃ£o**: O **NetHTTPClient** e o **Delphi Client** utilizam as APIs de sistema do Windows (WinHTTP e WinInet, respectively) e se beneficiam do pooling de conexÃ£o nativo do SO, geralmente resultando em respostas mais rÃ¡pidas em conexÃµes repetidas.
3. **Indy Engine**: Apresenta excelente robustez e controle granular de sockets, sendo ideal para sistemas multiplataforma legados que exigem implementaÃ§Ãµes independentes de bibliotecas de sistema operacional, embora possa ter uma variaÃ§Ã£o ligeiramente maior na latÃªncia devido Ã  ausÃªncia de pooling de socket automÃ¡tico agressivo nativo do Windows em iteraÃ§Ãµes curtas sem keep-alive explÃ­cito.

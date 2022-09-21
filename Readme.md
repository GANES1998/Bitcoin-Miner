## Erlang Bitcoin Mining Simulation.

This is the actor model based bitcoin mining system in erlang submitted in response to [project 1](https://ufl.instructure.com/courses/467300/assignments/5383668) of [COP5615](https://ufl.instructure.com/courses/467300).

#### Team

| Name                     | Gator Email             | UF Id     | Github username |
|--------------------------|-------------------------|-----------|-----------------|
| Ravichandran, Ganeson    | g.ravichandran@ufl.edu  | 1079-8982 | GANES1998       |
| Munaga, Sai Pavan Kalyan | saipavan.munaga@ufl.edu | xxxx-xxxx |                 |

#### Architecture

```mermaid
graph LR;
A[supervisor] --spawns--> B[worker];
A --spawns--> C[worker];
A --spawns--> D[worker];
A --spawns--> E[worker];
A --spawns--> F[worker];
C --Result Message--> G[Event Bus];
B --Result Message--> G[Event Bus];
D --Result Message--> G[Event Bus];
E --Result Message--> G[Event Bus];
F --Result Message--> G[Event Bus];
G --Dispatch--> A
```

- Supervisor receives K, the expected number of leading zeros in the hash for mining and spawns out a number of workers.
- Each worker computes and shares the result, success if a said hash with K leading zeros is found or nosuccess if such hash is not found.

#### Architecture of Supervisor

```mermaid
graph TB;
    A[K zeros] --User Input--> B[Supervisor];
    B ---> C[Calclate Max Propcess Allowed];
    D[Environment Var] --Processes/Core--> C;
    C ---> E[Spawn Processes];
    E ---> F[Listen for Events];
    F --loop--> F;
    H[Workers] --Resul tMessage--> F; 
    F ---> I{Message Type};
    I -->|Yes| J[Update Counts, print result];
    I -->|No| K[No Change to state]
```
#### Architecture of Worker
```mermaid
graph TR;

```
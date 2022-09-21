## Erlang Bitcoin Mining Simulation.

This is the actor model based bitcoin mining system in erlang submitted in response
to [project 1](https://ufl.instructure.com/courses/467300/assignments/5383668)
of [COP5615](https://ufl.instructure.com/courses/467300).

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
- Each worker computes and shares the result, success if a said hash with K leading zeros is found or nosuccess if such
  hash is not found.

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
graph TB;
    A[Initialized After getting spun] ---> B[Get Work Unit];
    C[Environment Vars] -.-> B;
    B ---> D[Loop];
    D -.-> E[Generate Random Str];
    C -.-> |Random Char Count| E;
    E ---> F[Generate Input String];
    G[User Input] -.-> |User Id| F;
    F ---> H[Sha256 Hash];
    H ---> I{Leading Zeros Check};
    G -.-> |K - Min Leading Zeros| I;
    I ---> |Yes| J[Send Message to Caller with Input String and Hash];
    I ---> |No - Loop for maximum of work unit times| D;
```

#### Answers

1. Size of the work unit that you determined results in the best performance for your implementation and an explanation
   of how you determined it. The size of the work unit refers to the number of sub-problems that a worker gets in a
   single request from the boss.

I tried multiple work units from 10, 50, 100, 200 and appropriately reducing the process per core so that the attempts
remain the same.
The corresponding wall clock times were used to determine the best value of Work Unit. The results of this experiment
are tabulated below.

| Work Unit | Process Per Core | Cores | Max Process Spun | Max Attempts | Bitcoins Mined | Wall Clock Time (ms) |
|-----------|------------------|-------|------------------|--------------|----------------|----------------------|
| 10        | 25000            | 8     | 200000           | 2000000      | 40             | 274503               |
| 50        | 5000             | 8     | 40000            | 2000000      | 27             | 15830                |
| 100       | 2500             | 8     | 20000            | 2000000      | 24             | 13671                |
| 200       | 1250             | 8     | 10000            | 2000000      | 25             | 12060                |
| 500       | 550              | 8     | 2000             | 2000000      | 20             | 11706                |

So, when the work unit 50, we have a perfect balance of Bitcoins mined and Time Taken. So, I have chosen 50 as the work
unit.

2. The result of running your program for input 4

From the above experiment, the work unit was set to **50** and hence the process per core was set to 5000. The result of
the program is shown below.

| Work Unit | Process Per Core | Cores | Max Process Spun | Max Attempts | Bitcoins Mined | Wall Clock Time (ms) |
|-----------|------------------|-------|------------------|--------------|----------------|----------------------|
| 50        | 10000            | 8     | 80000            | 4000000      | 68             | 62927                |

![](doc/assets/2/2_1.png)
![](doc/assets/2/2_2.png)

3. The running time for the above is reported by time for the above and report the time. The ratio of CPU time to REAL
   TIME tells you how many cores were effectively used in the computation. If you are close to 1 you have almost no
   parallelism (points will be subtracted).



4. The coin with the most 0s you managed to find.
   We were able to mine a coin with 6 leading zeros. The run is as follows.

| Work Unit | Process Per Core | Cores | Max Process Spun | Max Attempts | Bitcoins Mined | Wall Clock Time (ms) |
|-----------|------------------|-------|------------------|--------------|----------------|----------------------|
| 50        | 50000            | 8     | 400000           | 20000000     | 2              | 1509781              |

![](doc/assets/MaxLeadingZeros.png)

5. The largest number of working machines you were able to run your code with.
   Since, we are a group of two persons, I tried using working machines.

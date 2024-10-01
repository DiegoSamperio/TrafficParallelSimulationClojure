# Traffic Simulation in Clojure

This project simulates vehicle flow through a crossroad using data from text files that specify car arrivals and traffic logic. It uses parallel programming to optimize performance by approximately 25%, making the processing of multiple crossroads more efficient.

## Features

- **Traffic Flow Analysis:** Simulates traffic flow and calculates average wait times for cars at each intersection.
- **Dead Time Tracking:** Monitors and logs periods where no cars pass during green light cycles.
- **Detailed Statistics:** Provides overall and lane-specific performance metrics, including waiting times and dead time.
- **Parallel Processing:** Improves performance by processing multiple crossroads simultaneously, leading to around 25% optimization.

## How to Run

1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/Traffic-Simulation-Clojure.git

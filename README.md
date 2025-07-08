# 🌦️ Parametric Weather Insurance (PWI) Smart Contract 🌦️

A decentralized parametric insurance protocol that automatically triggers payouts based on weather events like rainfall, drought, temperature, wind speed, and humidity.

## 🚀 Overview

PWI enables users to create insurance policies that automatically pay out when specific weather conditions are met. The system uses trusted oracles to provide weather data, making it a transparent and efficient solution for weather-related risk management.

## ✨ Features

- 🔐 Create weather insurance policies with customizable parameters
- 🌧️ Support for multiple weather conditions (rainfall, drought, temperature, etc.)
- 📊 Oracle-based weather data feeds
- 💰 Automatic claim verification and payout
- 🛡️ Policy management (creation, cancellation)
- 📈 Transparent fee structure

## 📋 How It Works

1. **Policy Creation**: Users create policies by specifying:
   - Premium amount (in STX)
   - Coverage amount
   - Policy duration
   - Location identifier
   - Weather condition type
   - Threshold value for triggering a claim

2. **Weather Data**: Trusted oracles submit weather data for different locations

3. **Claims**: When weather conditions meet or exceed the specified threshold, policyholders can claim their coverage

## 🛠️ Contract Functions

### Policy Management

- `create-policy`: Create a new insurance policy
- `cancel-policy`: Cancel an existing policy (partial refund)
- `claim-policy`: Submit a claim for a valid policy

### Oracle Functions

- `submit-weather-data`: Oracle submits weather data for a location
- `set-oracle`: Admin function to set the trusted oracle address

### Administrative Functions

- `update-policy-duration-limits`: Update min/max policy duration
- `update-protocol-fee`: Update the protocol fee percentage

### Read-Only Functions

- `get-policy`: Get details about a specific policy
- `get-weather-data`: Get weather data for a location at a specific block height
- `get-latest-weather-data`: Get the most recent weather data for a location
- `get-contract-info`: Get general contract information

## 🧪 Example Usage

### Creating a Policy

```clarity
(contract-call? .pwi create-policy 
  u1000000 ;; premium: 1 STX
  u5000000 ;; coverage: 5 STX
  u4320    ;; duration: 4320 blocks (approx. 30 days)
  "new-york-10001" ;; location ID
  "rainfall"       ;; weather condition
  u100             ;; threshold: 100mm of rain
)
```

### Claiming a Policy

```clarity
(contract-call? .pwi claim-policy u1) ;; policy ID 1
```

## ⚠️ Requirements

- Clarity smart contract language
- Stacks blockchain
- Clarinet for local development and testing

## 🔒 Security Considerations

- Oracle data is critical - only trusted oracles should be allowed
- Policy parameters should be carefully chosen to avoid exploitation
- Contract funds are protected through proper authorization checks

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📜 License

This project is licensed under the MIT License.


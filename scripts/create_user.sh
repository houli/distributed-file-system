#!/bin/bash

echo "Creating an account"
read -p "Username: " username
read -s -p "Password: " password
echo -e "\n"

curl -H "Content-Type: application/json" -d "{\"username\": \"${username}\", \"password\": \"${password}\"}" http://localhost:8080/register

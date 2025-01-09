# Frontend Dockerfile
FROM node:18 AS build

# Set the working directory
WORKDIR /app

# Copy package.json and install dependencies
COPY package.json ./
RUN npm install

# Copy all project files
COPY . .

# Build the React app for production
CMD ["npm", "start"]


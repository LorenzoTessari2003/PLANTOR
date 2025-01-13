# Backend Dockerfile
FROM python:3.10-slim

# Set the working directory
WORKDIR /app

# Install Python dependencies
COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY ../.. .

# Expose the backend port
EXPOSE 5000

# Start the Flask server
CMD ["python", "run.py"]

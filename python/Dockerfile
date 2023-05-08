# THIS DOCKERFILE IS USED TO UPLOAD HEIZSCHEDULER_BASE image


# Use an official Python runtime as a parent image
FROM python:3.10-slim-buster

# Install SWI-Prolog
RUN apt-get update && \
    apt-get install -y swi-prolog && \
    rm -rf /var/lib/apt/lists/*

# Set the working directory to /app
WORKDIR /app

# Copy the requirements file to the working directory
COPY ./requirements.txt .

# Install the dependencies
RUN pip install -r requirements.txt --cache-dir /pip-cache


FROM quay.io/jupyter/datascience-notebook:2023-12-25

# Switch to root to perform operations requiring elevated privileges
USER root

# Add a new user 'lonomia' and set a password
RUN useradd -m lonomia && \
    echo "lonomia:your_password_here" | chpasswd && \
    adduser lonomia sudo

# Allow 'lonomia' to run commands without asking for a password (optional)
RUN echo 'lonomia ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

# Change ownership of the conda directory to 'lonomia'
RUN chown -R lonomia:lonomia /opt/conda

# Switch back to the new user
USER lonomia
ENV HOME /home/lonomia
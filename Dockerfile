FROM gibiansky/ihaskell

# cut here

USER root

RUN cabal update && \
    cabal install \
    ihaskell-display \
    ihaskell-blaze

USER jovyan

# cut here

COPY --chown=jovyan notebooks/ /home/jovyan/src/

COPY --chown=jovyan config/ /home/jovyan/.jupyter/

EXPOSE 8888

CMD jupyter-lab --ip=0.0.0.0

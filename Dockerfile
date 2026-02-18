FROM debian:bookworm-slim

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
     gnucobol \
     bash \
     coreutils \
     diffutils \
     findutils \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

RUN set -e; \
  if [ -f /app/InCollege.cob ]; then SRC=/app/InCollege.cob; \
  elif [ -f /app/src/incollege.cob ]; then SRC=/app/src/incollege.cob; \
  else echo "InCollege.cob not found"; exit 1; fi; \
  INCLUDES=""; \
  for d in /app /app/src /app/copybooks; do \
    [ -d "$d" ] && INCLUDES="$INCLUDES -I $d"; \
  done; \
  cobc -x -free -o /app/incollege $INCLUDES "$SRC"

CMD ["/app/incollege"]

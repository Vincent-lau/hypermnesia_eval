FROM erlang
WORKDIR /app
COPY ~/otp/lib/mnesia/examples/bench .
RUN make all
EXPOSE 4369

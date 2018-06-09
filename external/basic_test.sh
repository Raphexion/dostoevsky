url="http://localhost:7070"
callback_url1="http://localhost:7071"
callback_url2="http://localhost:7072"

# post in a couple of messages to topic "abc"

for ii in {1..2}
do
  curl -d '{"abc": {"ii": '${ii}'}}' ${url}/pub
done

# attach

curl -d '{"topic": "abc", "url": "'${callback_url1}'"}' ${url}/sub

# send some more messages to topic

for ii in {11..12}
do
  curl -d '{"abc": {"ii": '${ii}'}}' ${url}/pub
done

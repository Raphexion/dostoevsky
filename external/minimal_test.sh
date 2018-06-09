url="http://localhost:7070"
callback_url1="http://localhost:7071"

curl -d '{"abc": {"ii": 0}}' ${url}/pub
curl -d '{"topic": "abc", "url": "'${callback_url1}'"}' ${url}/sub
curl -d '{"abc": {"ii": 1}}' ${url}/pub

url="http://localhost:7070"


for ii in {0..10}
do
  curl -d '{"topic": "abc", "url": "url'${ii}'"}' ${url}/sub
  curl -d '{"url": "url'${ii}'", "topic": "abc"}' ${url}/sub
done

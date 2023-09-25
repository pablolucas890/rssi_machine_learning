import subprocess
import paho.mqtt.client as paho  # pip install paho-mqtt
import sys
import time
import os
import csv

# Consts
topic_prefix = 'rssi'


# Functions
def on_connect(client, device, flags, result_code):
    topic = topic_prefix
    print("Connected to MQTT broker, subscribing to topic " + topic)
    mqttc.subscribe(topic, 0)


def on_disconnect(client, device, rc):
    print("OOOOPS! MQTT disconnection")
    time.sleep(10)



def on_message(client, device, msg):
    message = msg.payload.decode('utf-8').lower()
    # print("Novo dado: " + message)
    with open('real_time.csv', 'r', newline='') as arquivo_csv:
        leitor_csv = csv.reader(arquivo_csv)
        linhas = list(leitor_csv)

    if len(linhas) >= 2:
      del linhas[1]

    nova_linha = [message]
    linhas.append(nova_linha)

    with open('real_time.csv', 'w', newline='') as arquivo_csv:
        escritor_csv = csv.writer(arquivo_csv)
        escritor_csv.writerows(linhas)


def predict():
  while True:
      resultado = subprocess.run(["Rscript", "predict.R"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

      linhas_saida = resultado.stdout.splitlines()

      ultimas_linhas = linhas_saida[-2:-1]

      if(ultimas_linhas[0][4:] == '0'):
          print("Não invadido")
      elif(ultimas_linhas[0][4:] == '1'):
          print("Invasão")


if __name__ == '__main__':

  # Initialise MQTT broker connection
  clientid = 'rssi-%s' % os.getpid()
  mqttc = paho.Client(clientid, False)
  mqttc.on_message = on_message
  mqttc.on_connect = on_connect
  mqttc.on_disconnect = on_disconnect
  mqttc.username_pw_set('', '')

  while True:
          try:
              mqttc.connect('localhost', 1883, 60)
              mqttc.loop_start()
              predict()
          except (OSError) as e:
              print(
                  "Cannot connect (error: %s), will try to reconnect in 5 seconds" % str(e))
              time.sleep(5)
          except KeyboardInterrupt:
              sys.exit(0)
          except:
              print("Error")

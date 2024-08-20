#Library
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
import time

#Abrir
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()))

url = 'https://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-de-segmentos-e-setoriais/indice-fundos-de-investimentos-imobiliarios-ifix-composicao-da-carteira.htm'
driver.get(url)


WebDriverWait(driver, 10).until(
    EC.frame_to_be_available_and_switch_to_it((By.ID, "bvmf_iframe")))

planilha = driver.find_element(By.XPATH, '//*[@id=\"divContainerIframeB3\"]/div/div[1]/form/div[2]/div/div[2]/div/div/div[1]/div[2]/p/a')

#Baixar
driver.execute_script("arguments[0].click();", planilha)
time.sleep(10)

#Fechar
driver.quit()



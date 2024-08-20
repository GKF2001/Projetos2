import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
import time


driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()))

#Abrir
url = 'https://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-de-segmentos-e-setoriais/indice-fundos-de-investimentos-imobiliarios-ifix-composicao-da-carteira.htm'
driver.get(url)

WebDriverWait(driver, 10).until(
    EC.frame_to_be_available_and_switch_to_it((By.ID, "bvmf_iframe"))
)

#Mudar Tamanho
select = Select(WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.ID, "selectPage"))
))
select.select_by_visible_text("120")  #120

time.sleep(5)  

table = WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.TAG_NAME, 'table'))
)
table_html = table.get_attribute('outerHTML')

#Fechar
driver.quit()

df = pd.read_html(table_html)[0]

#Arrumar decimais
df['Part. (%)'] = df['Part. (%)'] / 1000
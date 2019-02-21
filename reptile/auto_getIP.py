# -*- coding: utf-8 -*-
"""
Created on Sat Mar 03 19:06:18 2018
@author: Administrator
"""

import urllib3
import re
import requests
import time
from threading import Thread
from threading import Lock
import queue

# 从西刺抓下来的所有代理ip
all_find_list = []
# 将所有抓到的代理压入队列，四个线程可以从队列中获取代理ip
gaoni_queue = queue.Queue()
# 能够成功连接的代理ip
success_list = []

lock = Lock()


def get_proxy(checking_ip):
    # 根据得到的代理ip，设置proxy的格式
    proxy_ip = 'http://' + checking_ip
    proxy_ips = 'https://' + checking_ip
    proxy = {'https': proxy_ips, 'http': proxy_ip}
    return proxy


def checking_ip():
    global gaoni_queue
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36'
    }
    i=0
    while i<100:
        # 若从队列1秒内无法获得代理ip，说明所有代理均已检测完成，抛出Empty异常
        i=i+1
        print(i)
        try:
            checking_ip = gaoni_queue.get(True, 1)
        except:
            gaoni_queue.task_done()
            break

        proxy = get_proxy(checking_ip)
        url = 'https://www.baidu.com/'
        # 使用上面的url，测试代理ip是否能够链接
        try:
            page = requests.get(url, headers=headers, proxies=proxy)
        except:
            lock.acquire()
            print(checking_ip, '失败')
            lock.release()
        else:
            lock.acquire()
            print (checking_ip, '成功')
            success_list.append(checking_ip)
            lock.release()


def get_all():
    headers = {
        'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36'}
    global all_find_list
    for i in range(1, 2):
        # 从xici网站的高匿页面获取ip
        url = 'http://www.xicidaili.com/nn/%d' % i
        r = requests.get(url, headers=headers)
        data = r.text
        # 抓取所需数据的正则表达式
        p = r'<td>(.*?)</td>\s+<td>(.*?)</td>\s+<td>\s+(.*?)\s+</td>\s+<td class="country">(.*?)</td>'
        find_list = re.findall(p, data)
        all_find_list += find_list
    # 将ip地址与端口组成规定格式
    for row in all_find_list:
        ip = row[0] + ':' + row[1]
        gaoni_queue.put(ip)


if __name__ == '__main__':
    get_all()
    file = "ip.txt"
    print(gaoni_queue.qsize())
    checking_ip()
    f = open(file, "w")
    for row in success_list:
        f.write(row + '\n')
    f.close()

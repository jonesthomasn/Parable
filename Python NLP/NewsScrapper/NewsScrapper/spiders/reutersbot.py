# -*- coding: utf-8 -*-
import scrapy
from scrapy.selector import Selector
from stack.items import StackItem


class ReutersbotSpider(scrapy.Spider):
    name = "reutersbot"
    allowed_domains = ["https://www.reuters.com"]
    start_urls = ['http://https://www.reuters.com/finance/deals/mergers/','https://www.reuters.com/news/']


    def parse(self, response):
        headlines = Selector(response).xpath('//div[@class="feature"]/h2')
        for headline in headlines:
            item = StackItem()
            item['title'] = headline.xpath(
                'a/text()').extract()[0]
            item['url'] = headline.xpath(
                'a/@href').extract()[0]
            yield item

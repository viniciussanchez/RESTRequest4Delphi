unit Mercado.Libre.Consts;

interface

const
  ML_APIBASE = 'https://api.mercadolibre.com';

  /// <summary>
  /// Cada vez que realize a chamada que muda o code por um ACCESS_TOKEN,
  /// também terá o dado de um refresh_token, que deverá guardar para
  /// trocá-lo por um ACCESS_TOKEN uma vez expirado.
  /// Para renovar seu ACCESS_TOKEN deverá realizar a chamada seguinte:
  /// </summary>
  ML_POST_REFRESHTOKEN = 'oauth/token?grant_type=refresh_token&client_id={app_id}&client_secret={secret_key}&refresh_token={refresh_token}';

  ML_GET_CATEGORIES_SITE = '/sites/MLB/categories';
  ML_GET_CATEGORIES_INFO = '/categories/{category_id}';
  ML_GET_CATEGORIE_ATTRIBUTTES = '/categories/{category_id}/attributes';
  ML_GET_USERTEST = '/users/test_user';
  ML_GET_MODOSENVIO = '/users/{user_id}';
  ML_GET_LISTING_TYPES = '/sites/MLB/listing_types';
  ML_POST_ITEM = '/items';
  ML_GET_ITEM = '/items/{item_id}';
  ML_PUT_ITEM = '/items/{item_id}';
  ML_POST_ITEM_RELIST = '/items/{item_id}/relist';
  ML_UPLOAD_PICTURE = '/pictures';

  /// <summary>
  /// Recupera a comissão associada ao tipo de publicação e categoria
  /// </summary>
  ML_LISTING_PRICES_LIST_TYPE_CATEGORY = '/sites/MLB/listing_prices/{listing_type}?category_id={category_id}&price={price}';

  /// <summary>
  /// Calcular custos de frete grátis antes de publicar um item
  /// Utilize esse recurso para calcular os custos de frete grátis antes da publicação. Para isso, é necessario informar alguns parâmetros:
  /// </summary>
  ML_SHIPPING_OPTIONS = '/users/{user_id}/shipping_options/free?currency_id=BRL&listing_type_id={listing_type_id}&condition=new&category_id={category_id}&item_price={item_price}&verbose=true&dimensions={dimensions}';

  /// <summary>
  /// Dimensões produtos tipos de envios
  /// Você pode conhecer, por meio de uma chamada GET, as dimensões padrão da categoria
  /// de envios (peso, altura, largura e profundidade) e os tipos de logística disponíveis nessa categoria.
  // Quando as dimensões do produto não são especificadas, você deve executar o cálculo com base nas dimensões padrão.
  /// </summary>
  ML_SHIPPING_DIMENSION = '/categories/{category_id}/shipping_preferences';

  {$REGION 'Gerenciamento de Vendas'}
  /// <summary>
  /// Receba uma ordem
  /// Quando uma nova ordem é criada no usuário, os detalhes podem ser consultados
  /// através de uma solicitação ao recurso de ordens:
  /// </summary>
  ML_GET_ORDER = '/orders/{order_id}';

  /// <summary>
  /// Para realizar o faturamento de uma venda, são necessários os dados do comprador que estão disponíveis através do recurso Orders,
  /// mas específicamente /orders/order_id/billing_info.
  /// </summary>
  ML_GET_ORDER_BILLING_INFO = '/orders/{order_id}/billing_info';
  {$ENDREGION}

  /// <summary>
  /// Consultar status do envio
  /// Conhecendo o ID do envio, poderá realizar o GET para o recurso de shipments para obter toda a informação dele:
  /// </summary>
  ML_GET_SHIPIMENT = '/shipments/{shipping_id}';

  /// <summary>
  /// Billing Info
  /// No recurso/shipments são derivados diferentes recursos que oferecem a informação necessária para que o vendedor determine a logística.
  /// O recurso billing_info permite obter a informação fiscal dos diferentes atores que operam no envio do produto.
  /// </summary>
  ML_GET_SHIPIMENT_BILLING_INFO = '/shipments/{shipping_id}/billing_info';

  {$REGION 'Usuários e Aplicativos - Os seguintes recursos servirão para trabalhar com usuários e aplicativos registrados no Mercado Livre'}
  /// <summary> Informação da conta do usuário.</summary>
  ML_GET_USR_INF = '/users/{cust_id}';
  ML_GET_USR_INF_REGEX = '/users/:cust_id';
  /// <summary> Obtém a informação do usuário que fez login na conta.</summary>
  ML_GET_USR_LOGIN = '/users/me';
  /// <summary> Obtém endereço associados à conta do usuário.</summary>
  ML_GET_USR_ADDRESSES = '/users/{cust_id}/addresses';
  /// <summary> Obtém os métodos de pagamento aceitos pelo vendedor para cobrar.</summary>
  ML_GET_USR_ACCEPTED_PAYMENT_METHODS = '/users/{cust_id}/accepted_payment_methods';
  /// <summary> Obtém dados sobre o aplicativo.</summary>
  ML_GET_APP_INF = '/applications/{application_id}';
  /// <summary> Este processo recupera marcas associadas a um user_id. O atributo oficial_store identifica uma loja.</summary>
  ML_GET_USR_BRANDS = '/users/{user_id}/brands';
  /// <summary> Obtém informação dos pacotes de promoção para o usuário. IMÓVEIS</summary>
  ML_GET_USR_CLASSIFIEDS_PROMOTION_PACKS = '/users/{user_id}/classifieds_promotion_packs';
  /// <summary> Obter a disponibilidades para o usuário à listagem debaixo de um determinado tipo de venda e de categoria.</summary>
  ML_GET_USR_CLASSIFIEDS_PROMOTION_PACKS_EXT = '/users/{user_id}/classifieds_promotion_packs/{listing_type}&categoryId={category_id}';
  /// <summary> Listing types disponivéis por usuários e categorias.</summary>
  ML_GET_USR_AVAILABLE_LISTING_TYPES = '/users/{Cust_id}/available_listing_types?category_id={Category_id}';
  /// <summary> Obter o listing types disponível baixo um tipo de listagem segundo uma categoria outorgada.</summary>
  ML_GET_USR_AVAILABLE_LISTING_TYPES_EXT = '/users/{Cust_id}/available_listing_type/{listing_type_id}?category_id={Category_id}';
  /// <summary> Revogar Permissão do aplicativo</summary>
  ML_DELETE_USR_APPLICATIONS = '/users/{user_id}/applications/{app_id}';
  /// <summary> Histórico de notificações.</summary>
  ML_GET_MYFEEDS = '/myfeeds?app_id={app_id}';
  {$ENDREGION}

implementation

end.

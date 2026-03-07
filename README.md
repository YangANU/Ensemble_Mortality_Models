<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/YangANU/Ensemble_Mortality_Models">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>

<h3 align="center">Enhancing Mortality Forecasting with Ensemble Learning: A Shapley-Based Approach</h3>

  <p align="justify">
    A well-established insight in mortality forecasting is that combining predictions from a set of models improves accuracy compared to relying on a single best model. This paper proposes a novel ensemble approach based on Shapley values, a game-theoretic measure of each model’s marginal contribution to the forecast. We further compute these SHapley Additive exPlanations (SHAP)-based weights age-by-age, thereby capturing the specific contribution of each model at each age. In addition, we introduce a threshold mechanism that excludes models with negligible contributions, effectively reducing the forecast variance. Using data from 24 OECD countries, we demonstrate that our SHAP ensemble enhances out-of-sample forecasting performance, especially at longer horizons. By leveraging the complementary strengths of different mortality models and filtering out those that add little predictive power, our approach offers a robust and interpretable solution for improving mortality forecasts. <a href="https://github.com/YangANU/Ensemble_Mortality_Models"><strong>Explore R code »</strong></a>
    <br />
  </p>
</div>

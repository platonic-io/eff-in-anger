package aleryo;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class PaymentApplication extends Application<PaymentConfiguration> {

    public static void main(final String[] args) throws Exception {
        new PaymentApplication().run(args);
    }

    @Override
    public String getName() {
        return "Payment";
    }

    @Override
    public void initialize(final Bootstrap<PaymentConfiguration> bootstrap) {
        // TODO: application initialization
    }

    @Override
    public void run(final PaymentConfiguration configuration,
                    final Environment environment) {
        final PaymentResource resource = new PaymentResource();
        environment.jersey().register(resource);
    }

}
